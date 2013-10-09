package cc.factorie.app.nlp.hcoref1

import scala.annotation.tailrec
import scala.collection.mutable
import cc.factorie.variable.ArrowVariable

/**
 * @author harshal, Jack Sullivan
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
  final def isEntity:Boolean = !parent.isDefined

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