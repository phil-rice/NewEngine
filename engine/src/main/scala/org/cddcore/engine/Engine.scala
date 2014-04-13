package org.cddcore.engine

trait Engine[Params, BFn, R, RFn]

trait Engine2[P1, P2, R] extends Engine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] with DecisionTree2[P1, P2, R]

trait Engine3[P1, P2, P3, R] extends Engine[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R] with DecisionTree3[P1, P2, P3, R] 

