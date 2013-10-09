package cc.factorie.app.nlp.hcoref1

import cc.factorie.variable.ArrowVariable
import scala.annotation.tailrec
import scala.collection.mutable

/**
 * @author harshal
 * @date: 10/3/13
 */

abstract class Node[Vars <: NodeVariables[Vars]](private val variables:Vars) {

  type This = Node[Vars]
  type ThisMention = Mention[Vars]

  private val parentRef = new ArrowVariable[This, This](this, null)
  protected val _children:mutable.HashSet[This] = mutable.HashSet[This]()

  final def parent:Option[This] = Option(parentRef.dst)
  def children:Iterable[This] = _children
  def descendants:Iterable[This] = if(children.nonEmpty) { //todo make tailrec
    children.flatMap{_.descendants}
  } else {
    Nil
  }
  def leaves:Iterable[ThisMention] = descendants.collect{case desc:ThisMention => desc}

  @tailrec
  final def root:This = parent match {
    case Some(p) => p.root
    case None => this
  }

  final def size:Int = descendants.size
  final def depth:Int = {
    @tailrec
    def helper(pOpt:Option[This], cDepth:Int):Int = {
      pOpt match {
        case Some(p) => helper(p.parent, cDepth + 1)
        case None => cDepth
      }
    }
    helper(parent,0)
  }

  //todo do we even need these?
  protected def leavingParent(parent:This)
  protected def joiningParent(parent:This)

  final def alterParent(newParent:Option[This]) {
    val oldParent = this.parent
    oldParent match {
      case Some(oParent) => {
        leavingParent(oParent)
        oParent.variables remove this.variables
        oParent._children -= this
      }
      case None => Unit
    }
    newParent match {
      case Some(nParent) => {
        joiningParent(nParent)
        nParent.variables combine this.variables
        parentRef.set(nParent)(null) // todo we probably do want a difflist here
        nParent._children += this
      }
      case None => parentRef.set(null)(null) // todo we probably do want a difflist here
    }
  }
}

trait NodeVariables[Self <: NodeVariables[Self]] {
  this :Self =>

  def combine(other:Self):Self
  def remove(other:Self):Self
}

abstract class Mention[Vars <: NodeVariables[Vars]](v:Vars) extends Node[Vars](v) {
  override final val children = Nil
  override final val descendants = Nil
  override final val leaves = this :: Nil
}


/*
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

 */
