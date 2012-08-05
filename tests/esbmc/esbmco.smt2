(set-logic QF_AUFBV)
(set-option :produce-models true)
(declare-fun j () (_ BitVec 32))
(assert (= j (_ bv1 32)))
(declare-fun x () (_ BitVec 32))
(assert (= x (_ bv2 32)))
(declare-fun i () (_ BitVec 32))
(assert (= i (_ bv0 32)))
(declare-fun .str () (Array (_ BitVec 4) (_ BitVec 8)))
(declare-fun .str1 () (Array (_ BitVec 4) (_ BitVec 8)))
(declare-fun __PRETTY_FUNCTION__.Tx () (Array (_ BitVec 5) (_ BitVec 8)))
(declare-fun a () (Array (_ BitVec 4) (_ BitVec 32)))
(declare-fun id1 () (_ BitVec 64))
(declare-fun id2 () (_ BitVec 64))
(declare-fun arg1 () (_ BitVec 32))
(declare-fun arg2 () (_ BitVec 32))
(assert (= (_ bv10 32) arg1))
(assert (= (_ bv20 32) arg2))
(declare-fun tmp () (_ BitVec 32))
(assert (= tmp i))
(declare-fun tmp1 () (_ BitVec 8))
(assert (= tmp1 ((_ extract 7 0) arg1)))
(check-sat)
(exit)
