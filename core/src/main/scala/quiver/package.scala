/** Minimal quiver stub.
 *  Replaces `io.verizon.quiver` (a Haskell FGL port) which has no Scala 3 release.
 *  Provides enough of the API to compile the nelson routing subsystem.
 */
package object quiver {

  /** A labeled node: vertex identifier + label. */
  final case class LNode[N, A](vertex: N, label: A)

  /** A labeled edge: from + to vertex + label. */
  final case class LEdge[N, B](from: N, to: N, label: B)

  /** An adjacency entry: the edge label and the neighboring vertex key.
   *  Used in Context.inEdges / outEdges.
   */
  final case class Adj[+B, +N](label: B, to: N)

  /** The local context of a vertex during graph decomposition. */
  final case class Context[N, A, B](
    inEdges: Seq[Adj[B, N]],
    vertex: N,
    label: A,
    outEdges: Seq[Adj[B, N]]
  ) {
    def successors: Seq[N] = outEdges.map(_.to)
    def predecessors: Seq[N] = inEdges.map(_.to)
  }

  /** Result of decomposing a graph at a vertex: the context + the rest. */
  final case class Decomp[N, A, B](ctx: Option[Context[N, A, B]], rest: Graph[N, A, B])

  /** An immutable functional graph with labeled nodes and edges.
   *
   *  Backed by adjacency maps for O(log n) operations.
   *
   *  Type parameters:
   *    N - vertex key type
   *    A - node label type
   *    B - edge label type
   */
  final class Graph[N, A, B] private (
    private val nodeMap: Map[N, A],
    private val outAdj: Map[N, List[Adj[B, N]]],
    private val inAdj: Map[N, List[Adj[B, N]]]
  ) {
    def isEmpty: Boolean = nodeMap.isEmpty

    def order: Int = nodeMap.size

    /** All vertex keys in the graph. */
    def nodes: Seq[N] = nodeMap.keys.toSeq

    /** Outgoing edges from vertex v as Adj entries (edge-label, destination-vertex). */
    def outs(v: N): List[Adj[B, N]] = outAdj.getOrElse(v, Nil)

    /** Add a node (no-op if already present). */
    def addNode(n: LNode[N, A]): Graph[N, A, B] =
      new Graph(nodeMap + (n.vertex -> n.label), outAdj, inAdj)

    /** Add an edge. */
    def addEdge(e: LEdge[N, B]): Graph[N, A, B] = {
      val newOut = outAdj.updated(e.from, Adj(e.label, e.to) :: outAdj.getOrElse(e.from, Nil))
      val newIn  = inAdj.updated(e.to, Adj(e.label, e.from) :: inAdj.getOrElse(e.to, Nil))
      new Graph(nodeMap, newOut, newIn)
    }

    /** Remove a vertex and all its incident edges. */
    def removeNode(v: N): Graph[N, A, B] = {
      val newNodes = nodeMap - v
      val outs_ = outAdj.getOrElse(v, Nil).map(_.to)
      val ins_  = inAdj.getOrElse(v, Nil).map(_.to)
      val newOut = (outAdj - v).map { case (k, es) => k -> es.filterNot(_.to == v) }
      val newIn  = (inAdj - v).map { case (k, es) => k -> es.filterNot(_.to == v) }
      new Graph(newNodes, newOut -- outs_, newIn -- ins_)
    }

    /** Decompose the graph at vertex v. */
    def decomp(v: N): Decomp[N, A, B] =
      nodeMap.get(v) match {
        case None => Decomp(None, this)
        case Some(lbl) =>
          val ctx = Context(
            inEdges  = inAdj.getOrElse(v, Nil),
            vertex   = v,
            label    = lbl,
            outEdges = outAdj.getOrElse(v, Nil)
          )
          Decomp(Some(ctx), removeNode(v))
      }

    def successors(v: N): Seq[N] = outAdj.getOrElse(v, Nil).map(_.to)

    def predecessors(v: N): Seq[N] = inAdj.getOrElse(v, Nil).map(_.to)

    /** Map over vertex keys (requires f to be injective to preserve graph structure). */
    def vmap[M](f: N => M): Graph[M, A, B] =
      new Graph(
        nodeMap.map { case (k, v) => f(k) -> v },
        outAdj.map { case (k, es) => f(k) -> es.map(a => Adj(a.label, f(a.to))) },
        inAdj.map  { case (k, es) => f(k) -> es.map(a => Adj(a.label, f(a.to))) }
      )

    /** Map over node labels. */
    def nmap[C](f: A => C): Graph[N, C, B] =
      new Graph(nodeMap.map { case (k, v) => k -> f(v) }, outAdj, inAdj)

    /** Map over edge labels. */
    def emap[C](f: B => C): Graph[N, A, C] =
      new Graph(
        nodeMap,
        outAdj.map { case (k, es) => k -> es.map(a => Adj(f(a.label), a.to)) },
        inAdj.map  { case (k, es) => k -> es.map(a => Adj(f(a.label), a.to)) }
      )

    /** All nodes as labeled nodes. */
    def labNodes: Seq[LNode[N, A]] =
      nodeMap.map { case (k, v) => LNode(k, v) }.toSeq

    /** All edges as labeled edges. */
    def labEdges: Seq[LEdge[N, B]] =
      outAdj.flatMap { case (from, es) => es.map(a => LEdge(from, a.to, a.label)) }.toSeq

    /** True if the vertex is in the graph. */
    def member(v: N): Boolean = nodeMap.contains(v)

    def label(v: N): Option[A] = nodeMap.get(v)

    override def toString: String = s"Graph(nodes=${nodeMap.keys.mkString(",")}, edges=${labEdges.size})"
  }

  object Graph {
    def empty[N, A, B]: Graph[N, A, B] =
      new Graph(Map.empty, Map.empty, Map.empty)

    def mkGraph[N, A, B](ns: Seq[LNode[N, A]], es: Seq[LEdge[N, B]]): Graph[N, A, B] = {
      val g0 = ns.foldLeft(empty[N, A, B])((g, n) => g.addNode(n))
      es.foldLeft(g0)((g, e) => g.addEdge(e))
    }
  }

  /** Graphviz rendering stub — generates a minimal DOT representation. */
  object viz {
    sealed trait Orientation
    case object Portrait  extends Orientation
    case object Landscape extends Orientation

    def graphviz[N, A, B](
      g: Graph[N, A, B],
      title: String = "graph",
      pageSize: (Float, Float) = (8.5f, 11f),
      gridSize: (Int, Int) = (1, 1),
      orient: Orientation = Portrait
    ): String = {
      val sb = new StringBuilder
      sb.append(s"""digraph "$title" {\n""")
      g.labNodes.foreach(n => sb.append(s"""  "${n.vertex}";\n"""))
      g.labEdges.foreach(e => sb.append(s"""  "${e.from}" -> "${e.to}";\n"""))
      sb.append("}\n")
      sb.toString
    }
  }
}
