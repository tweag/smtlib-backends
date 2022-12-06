{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module SMTLIB.Backends.Tests.Sources (Source (..), sources) where

import SMTLIB.Backends

-- | A source is a list of SMTLib2 commands, available in several different formats
-- for testing purposes.
data Source = Source
  { -- | The name of the source.
    name :: String,
    -- | A computation equivalent to sending the raw content of the source to the
    -- solver.
    run :: Solver -> IO ()
  }

-- | A list of examples SMT-LIB files. Most of them were taken from the official
-- SMT-LIB website:
-- https://smtlib.cs.uiowa.edu/examples.shtml
sources =
  [ assertions,
    assignments,
    boolean,
    info,
    integerArithmetic,
    modelingSequentialCodeSSA,
    modelingSequentialCodeBitvectors,
    scopes,
    sorts,
    unsatCores,
    valuesOrModels
  ]

assertions = Source "assertions" $ \solver -> do
  ackCommand solver "(set-option :produce-assertions true)" 
  ackCommand solver "(set-logic QF_UF)" 
  ackCommand solver "(declare-const p Bool)"
  ackCommand solver "(declare-const q Bool)" 
  ackCommand solver "(push 1)" 
  ackCommand solver "(assert (or p q))"
  ackCommand solver "(push 1)" 
  ackCommand solver "(assert (not q))"
  _ <- command solver "(get-assertions)"
  ackCommand solver "(pop 1)" 
  _ <- command solver "(get-assertions)"
  ackCommand solver "(pop 1)" 
  _ <- command solver "(get-assertions)"
  ackCommand solver "(exit)" 

assignments = Source "assignments" $ \solver -> do
  ackCommand solver "(set-option :produce-assignments true)" 
  ackCommand solver "(set-logic QF_UF)" 
  ackCommand solver "(declare-const p Bool)" 
  ackCommand solver "(declare-const q Bool)" 
  ackCommand solver "(declare-const r Bool)" 
  ackCommand solver "(assert (not (=(! (and (! p :named P) q) :named PQ) (! r :named R))))" 
  _ <- command solver "(check-sat)"
  _ <- command solver "(get-assignment)"
  ackCommand solver "(exit)" 

boolean = Source "boolean" $ \solver -> do
  ackCommand solver "(set-logic QF_UF)" 
  ackCommand solver "(declare-const p Bool)" 
  ackCommand solver "(assert (and p (not p)))" 
  _ <- command solver "(check-sat)"
  ackCommand solver "(exit)" 

info = Source "info" $ \solver -> do
  _ <- command solver "(get-info :name)" 
  _ <- command solver "(get-info :version )" 
  _ <- command solver "(get-info :authors )" 
  ackCommand solver "(exit)" 

integerArithmetic = Source "integer arithmetic" $ \solver -> do
  ackCommand solver "(set-logic QF_LIA)" 
  ackCommand solver "(declare-const x Int)" 
  ackCommand solver "(declare-const y Int)" 
  ackCommand solver "(assert (= (- x y) (+ x (- y) 1)))" 
  _ <- command solver "(check-sat)" 
  ackCommand solver "(exit)" 

modelingSequentialCodeSSA = Source "modeling sequential code (SSA)" $ \solver -> do
  ackCommand solver "(set-logic QF_UFLIA)" 
  ackCommand solver "(set-option :produce-models true)" 
  ackCommand solver "(declare-fun x (Int) Int)" 
  ackCommand solver "(declare-fun y (Int) Int)" 
  ackCommand solver "(declare-fun t (Int) Int)" 
  ackCommand solver "(assert (= (t 0) (x 0)))" 
  ackCommand solver "(assert (= (y 1) (t 0)))" 
  ackCommand solver "(assert (= (x 1) (y 1)))" 

  ackCommand solver "(assert (not (and (= (x 1) (y 0)) (= (y 1) (x 0)))))" 
  _ <- command solver "(check-sat)" 
  _ <- command solver "(get-value ((x 0) (y 0) (x 1) (y 1)))" 
  _ <- command solver "(get-model)" 
  ackCommand solver "(exit)" 

modelingSequentialCodeBitvectors = Source "modeling sequential code (bitvectors)"
  $ \solver -> do
  ackCommand solver "(set-logic QF_BV)" 
  ackCommand solver "(set-option :produce-models true)" 
  ackCommand solver "(declare-const x_0 (_ BitVec 32))" 
  ackCommand solver "(declare-const x_1 (_ BitVec 32))" 
  ackCommand solver "(declare-const x_2 (_ BitVec 32))" 
  ackCommand solver "(declare-const y_0 (_ BitVec 32))" 
  ackCommand solver "(declare-const y_1 (_ BitVec 32))" 
  ackCommand solver "(assert (= x_1 (bvadd x_0 y_0)))" 
  ackCommand solver "(assert (= y_1 (bvsub x_1 y_0)))" 
  ackCommand solver "(assert (= x_2 (bvsub x_1 y_1)))" 
  ackCommand solver "(assert (not (and (= x_2 y_0) (= y_1 x_0))))" 
  _ <- command solver "(check-sat)" 
  ackCommand solver "(exit)" 

scopes = Source "scopes" $ \solver -> do
  ackCommand solver "(set-logic QF_LIA)" 
  ackCommand solver "(declare-const x Int)" 
  ackCommand solver "(declare-const y Int)" 
  ackCommand solver "(assert (= (+ x (* 2 y)) 20))" 
  ackCommand solver "(push 1)" 
  ackCommand solver "(assert (= (- x y) 2))" 
  _ <- command solver "(check-sat)" 
  ackCommand solver "(pop 1)" 
  ackCommand solver "(push 1)" 
  ackCommand solver "(assert (= (- x y) 3))" 
  _ <- command solver "(check-sat)" 
  ackCommand solver "(pop 1)" 
  ackCommand solver "(exit)" 

sorts = Source "sorts" $ \solver -> do
  ackCommand solver "(set-logic QF_UF)" 
  ackCommand solver "(declare-sort A 0)" 
  ackCommand solver "(declare-const a A)" 
  ackCommand solver "(declare-const b A)" 
  ackCommand solver "(declare-const c A)" 
  ackCommand solver "(declare-const d A)" 
  ackCommand solver "(declare-const e A)" 
  ackCommand solver "(assert (or (= c a)(= c b)))" 
  ackCommand solver "(assert (or (= d a)(= d b)))" 
  ackCommand solver "(assert (or (= e a)(= e b)))" 
  ackCommand solver "(push 1)" 
  ackCommand solver "(assert (distinct c d))" 
  _ <- command solver "(check-sat)" 
  ackCommand solver "(pop 1)" 
  ackCommand solver "(push 1)" 
  ackCommand solver "(assert (distinct c d e))" 
  _ <- command solver "(check-sat)" 
  ackCommand solver "(pop 1)" 
  ackCommand solver "(exit)" 

unsatCores = Source "unsat cores" $ \solver -> do
  ackCommand solver "(set-option :produce-unsat-cores true)" 
  ackCommand solver "(set-logic QF_UF)" 
  ackCommand solver "(declare-const p Bool)" 
  ackCommand solver "(declare-const q Bool)" 
  ackCommand solver "(declare-const r Bool)" 
  ackCommand solver "(declare-const s Bool)" 
  ackCommand solver "(declare-const t Bool)" 
  ackCommand solver "(assert (! (=> p q) :named PQ))" 
  ackCommand solver "(assert (! (=> q r) :named QR))" 
  ackCommand solver "(assert (! (=> r s) :named RS))" 
  ackCommand solver "(assert (! (=> s t) :named ST))" 
  ackCommand solver "(assert (! (not (=> q s)) :named NQS))" 
  _ <- command solver "(check-sat)" 
  _ <- command solver "(get-unsat-core)" 
  ackCommand solver "(exit)" 

valuesOrModels = Source "values or models" $ \solver -> do
  ackCommand solver "(set-option :produce-models true)" 
  ackCommand solver "(set-logic QF_LIA)" 
  ackCommand solver "(declare-const x Int)" 
  ackCommand solver "(declare-const y Int)" 
  ackCommand solver "(assert (= (+ x (* 2 y)) 20))" 
  ackCommand solver "(assert (= (- x y) 2))" 
  _ <- command solver "(check-sat)" 
  _ <- command solver "(get-value (x y))" 
  _ <- command solver "(get-model)" 
  ackCommand solver "(exit)" 
