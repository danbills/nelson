package nelson

import cats.Eval
import cats.effect.IO
import cats.free.Cofree
import cats.syntax.functor._

import fs2.{Pipe, Stream}

import quiver.{Context, Decomp, Graph}

import java.util.concurrent.TimeoutException

import scala.concurrent.duration.FiniteDuration
import scala.collection.immutable.LazyList

/** Compatibility type alias — fs2 3.x removed `Sink`; use `Pipe[F, A, Nothing]` instead. */
type Sink[F[_], -A] = Pipe[F, A, Nothing]

object CatsHelpers {
  extension [A](io: IO[A]) {
    /** Run `other` if this IO fails */
    def or(other: IO[A]): IO[A] = io.attempt.flatMap {
      case Right(a) => IO.pure(a)
      case Left(_)  => other
    }

    /** Fail with error if the result of the IO does not satisfy the predicate */
    def ensure(failure: => Throwable)(f: A => Boolean): IO[A] =
      io.flatMap(a => if (f(a)) IO.pure(a) else IO.raiseError(failure))

    /** Fail with TimeoutException if not completed within `timeout`. */
    def timed(timeout: FiniteDuration): IO[A] =
      io.timeout(timeout)
  }

  private def sinkW[F[_], W, O](actualSink: Pipe[F, W, Nothing]): Pipe[F, Either[W, O], Nothing] =
    stream => actualSink(stream.collect { case Left(e) => e })

  private def pipeO[F[_], W, O, O2](actualPipe: Pipe[F, O, O2]): Pipe[F, Either[W, O], Either[W, O2]] =
    _.flatMap {
      case Left(a)  => Stream.emit(Left(a))
      case Right(b) => actualPipe(Stream.emit(b)).map(Right(_))
    }

  extension [F[_], W, O](stream: Stream[F, Either[W, O]]) {
    def observeW(sink: Pipe[F, W, Nothing]): Stream[F, Either[W, O]] =
      stream.observe(sinkW(sink))

    def stripW: Stream[F, O] = stream.collect { case Right(o) => o }

    def throughO[O2](pipe: Pipe[F, O, O2]): Stream[F, Either[W, O2]] =
      stream.through(pipeO(pipe))
  }

  // ── Quiver graph helpers ──────────────────────────────────────────────────

  private type SStream[A] = LazyList[A]
  private type Tree[A] = Cofree[SStream, A]

  private def flattenTree[A](tree: Tree[A]): SStream[A] = {
    def go(tree: Tree[A], xs: SStream[A]): SStream[A] =
      LazyList.cons(tree.head, tree.tail.value.foldRight(xs)(go(_, _)))
    go(tree, LazyList.empty)
  }

  private def Node[A](root: A, forest: => SStream[Tree[A]]): Tree[A] =
    Cofree[SStream, A](root, Eval.later(forest))

  extension [N, A, B](graph: Graph[N, A, B]) {
    def reachable(v: N): Vector[N] =
      xdfWith(Seq(v), _.successors, _.vertex)._1.flatMap(flattenTree)

    def xdfWith[C](vs: Seq[N], d: Context[N, A, B] => Seq[N], f: Context[N, A, B] => C): (Vector[Tree[C]], Graph[N, A, B]) =
      if (vs.isEmpty || graph.isEmpty) (Vector(), graph)
      else graph.decomp(vs.head) match {
        case Decomp(None, g) => g.xdfWith(vs.tail, d, f)
        case Decomp(Some(c), g) =>
          val (xs, _) = g.xdfWith(d(c), d, f)
          val (ys, g3) = g.xdfWith(vs.tail, d, f)
          (Node(f(c), xs.to(LazyList)) +: ys, g3)
      }
  }
}
