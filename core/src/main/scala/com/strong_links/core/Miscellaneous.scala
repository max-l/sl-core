package com.strong_links.core

import com.strong_links.core._

import java.util.IdentityHashMap

// Thread-safe identity map for any objects. 
class IdentityMap[A <: AnyRef, B] {

  // Object on which the lock will be made.
  private val lock = new Object

  // Internal identity map
  private val h = new IdentityHashMap[A, B]

  // Service method.
  private def gput(key: A, once: Boolean)(compute: => B): B = lock synchronized {
    val exists = h.containsKey(key)
    if (exists && once)
      Errors.fatal("Duplicate key _ detected." << key)
    if (exists)
      h.get(key)
    else {
      val result = compute
      h.put(key, result)
      result
    }
  }

  // Put the key along with its value, as many times as we want.
  def put(key: A)(compute: => B): B = gput(key, false)(compute)

  // Put the key along with its value, only once.
  def putOnce(key: A)(compute: => B): B = gput(key, true)(compute)

  def getKeys = getKeysAndValues.map(_._1)

  def getValues = getKeysAndValues.map(_._2)

  def getKeysAndValues = lock synchronized {
    // Convert the Java set to Scala list. 
    // Note that the implicit conversions offered by Scala did not work
    // when these lines were written.
    val lb = scala.collection.mutable.ListBuffer[(A, B)]()
    val i = h.entrySet.iterator
    while (i.hasNext) {
      val x = i.next
      val tuple = (x.getKey, x.getValue)
      lb += tuple
    }
    lb.toList
  }

  def exists(key: A) = lock synchronized { h.containsKey(key) }

  def get(key: A) = lock synchronized { h.get(key) }
}

class UniqueIdentityMap[A <: AnyRef] extends com.strong_links.core.IdentityMap[A, A] {
  def put(key: A) = super.put(key)(key)
}

