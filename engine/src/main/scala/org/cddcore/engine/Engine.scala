package org.cddcore.engine

trait Engine[Params, BFn, R, RFn] {
  def evaluate(params: Params): R
}

object Engine {

}

