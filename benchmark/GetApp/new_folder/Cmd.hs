{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Calculator.Evaluator.Cmd (evalCmd) where

--------------------------------------------------------------------------------

import           Calculator.Evaluator.Expr      (evalExpr)
import           Calculator.Evaluator.Func      (execFunc)
import           Calculator.Help                (help)
import           Calculator.Prim.Bindings       (Bindings, addFun, addVar,
                                                 display)
import           Calculator.Prim.Cmd            (Cmd (..))
import           Calculator.Prim.Definitions    (defBinds)
import           Calculator.Prim.Expr           (Expr (Message, Constant))
import           Calculator.Prim.Function       (testFunc)
import           Calculator.Prim.Result         (Result (..))

--------------------------------------------------------------------------------

#ifdef PLOT

import           Calculator.Prim.Bindings       (getFun)
import           Calculator.Prim.Function       (Function, apply, arity)

import           Graphics.Rendering.Plot.Gtk.UI (plotWithArity)
import           Graphics.UI.Gtk                (initGUI, mainGUI)

import           Data.List                      (foldl1')
import           Data.Maybe

#endif

--------------------------------------------------------------------------------

evalCmd :: Bindings -> Cmd -> Result
evalCmd _ Help    = Text . unlines $ help
evalCmd b Display = Text . unlines . display $ b
evalCmd _ Reset   = NewBindings defBinds
evalCmd b (Assign s e)   =
    case evalExpr b e of
      Message xxs -> Error . unlines $ xxs
      Constant c  -> NewBindings . addVar (s, c) $ b
      _           -> Error " ~~ Erroneous Result ~~ "
evalCmd b fun@(Func i _ _) = let f = execFunc b fun
                             in case testFunc f 0 of
                                  Nothing -> NewBindings . addFun (i, f) $ b
                                  Just ms -> Error . unlines $ ms

--------------------------------------------------------------------------------

#ifdef PLOT

evalCmd b (Plot s rs) =
    case getFun s b of
      Nothing -> Error $ "Unknown function: " ++ s
      Just f  -> let pairMap fn (x, y) = (fn x, fn y)
                     rangeExps = map (pairMap (evalExpr b)) rs
                     rangeErrs = map (uncurry grabMessage) rangeExps
                     allErrors = foldl1' grabMessage rangeErrs
                     ranges    = map (pairMap (\(Constant c) -> c)) rangeExps
                     fromMsg   = (\(Message ms) -> ms)
                 in if null . fromMsg $ allErrors
                    then plotFunction f ranges
                    else Error . unlines . fromMsg $ allErrors

grabMessage :: Expr -> Expr -> Expr
grabMessage (Message m1)   (Message m2) = Message (m1 ++ m2)
grabMessage m1@(Message _) _            = m1
grabMessage _            m2@(Message _) = m2
grabMessage _              _            = Message []

plotFunction :: Function -> [(Double, Double)] -> Result
plotFunction f ranges =
    if length ranges /= arity f
    then Error $ "Invalid parameters: " ++
             "Required " ++ show (arity f) ++ " range(s). " ++
             "Provided " ++ show (length ranges) ++ "."
    else Action $ do
      _ <- initGUI
      fromJust $ plotWithArity (arity f) ((\(Right x) -> x) . apply f) ranges
      mainGUI

#endif

--------------------------------------------------------------------------------
