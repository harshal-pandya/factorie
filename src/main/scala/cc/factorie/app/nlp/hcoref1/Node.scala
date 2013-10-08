package cc.factorie.app.nlp.hcoref1

import cc.factorie.variable.ArrowVariable
import scala.annotation.tailrec
import cc.factorie.app.nlp.hcoref.SparseBagOfWords

/**
 * @author harshal
 * @date: 10/3/13
 */

trait Node[T <: Node[T,L], L <: LeafNode[L]] {
  this: T =>
  val parentRef = new ArrowVariable[T, Option[T]](this, None)
  def parent: Option[T] = parentRef.dst
  var _children: Seq[Node[_,_]] = _
  def children: Seq[Node[_,_]] = _children
  def descendants: Seq[T] = {
    def helper(n:T): Seq[T] = {
      if(n.children.isEmpty) Seq(n)
      n.children.foldLeft(Seq.empty[T])((seq,n)=> seq ++ helper(n))
    }
    helper(this)
  }
  def leaves: Seq[L] = descendants.flatMap {
      case leaf: L => Some(leaf)
      case _ => None
  }
  def addChild(c: Node[_,_]){
    _children = _children :+ c
  }

  @tailrec
  final def root: T = parent match { case Some(p) => p.root; case None => this }

  final def isRoot: Boolean = !this.parent.isDefined

  final def depth: Int = {
    def depthHelper(root: Option[T]):Int = {
      if(!parent.isDefined) 0
      else 1 + depthHelper(parent)
    }
    depthHelper(Some(this))
  }

  def size: Int = descendants.size

  def alterParent(p:Option[T]){
    val oldParent = parent
    val newParent = p.get
    //call hook
    p match{
      case Some(parent) => {
        newParent.addChild(this)
      }
    }
  }

  def alter(a:Any):PartialFunction[Any, Int] = {
    case a:Int => 1
  }
}


trait LeafNode[T <: LeafNode[T]] extends Node[LeafNode[T], T] {
  override val children = Nil
  override val size = 1
  override val descendants = Nil
  override val leaves = Seq(this)
}

class Mention extends LeafNode[Mention]

class InternalNode extends Node[InternalNode, Mention]


