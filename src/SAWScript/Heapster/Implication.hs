{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SAWScript.Heapster.Implication where

import Data.Maybe
import Data.List
import Data.Kind as Kind
import Data.Functor.Product
import Data.Functor.Compose
import GHC.TypeLits
import Control.Lens hiding ((:>))
import Control.Monad
import Data.Functor.Identity
import Control.Applicative hiding (empty)
import qualified Control.Applicative as Applicative
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Lang.Crucible.Types
import Lang.Crucible.LLVM.MemModel
import Lang.Crucible.CFG.Core
import Lang.Crucible.FunctionHandle
import Verifier.SAW.OpenTerm

import Data.Binding.Hobbits
import SAWScript.Heapster.CruUtil
import SAWScript.Heapster.Permissions


----------------------------------------------------------------------
-- * Permission Implications
----------------------------------------------------------------------

-- FIXME: add error messages to Impl_Fail, for debugging by the user

-- | A simple implication is an implication that does not introduce any
-- variables or act on the 'varPermMap' part of a permission set. (Compare to
-- the more general 'PermImpl'.) It has the form
--
-- > P1 * ... * Pn -o P1' * ... * Pm'
--
-- where the types of @P1@ through @Pn@ are given by the first type argument
-- @ps_in@ and those of @P1'@ through @Pm'@ are given by the second, @ps_out@.
data SimplImpl ps_in ps_out where
  SImpl_Drop :: ExprVar a -> ValuePerm a -> SimplImpl (RNil :> a) RNil
  -- ^ Drop a permission, i.e., forget about it:
  --
  -- > x:p -o .

  SImpl_Swap :: ExprVar a -> ValuePerm a -> ExprVar b -> ValuePerm b ->
                SimplImpl (RNil :> a :> b) (RNil :> b :> a)
  -- ^ Swap the top two permissions on the stack:
  --
  -- > x:p1 * y:p2 -o y:p2 * x:p1

  SImpl_IntroOrL :: ExprVar a -> ValuePerm a -> ValuePerm a ->
                    SimplImpl (RNil :> a) (RNil :> a)
  -- ^ @SImpl_IntroOrL x p1 p2@ applies left disjunction introduction:
  --
  -- > x:p1 -o x:(p1 \/ p2)

  SImpl_IntroOrR :: ExprVar a -> ValuePerm a -> ValuePerm a ->
                    SimplImpl (RNil :> a) (RNil :> a)
  -- ^ @SImpl_IntroOrR x p1 p2 pf@ applies right disjunction introduction:
  --
  -- > x:p2 -o x:(p1 \/ p2)

  SImpl_IntroExists :: KnownRepr TypeRepr tp => ExprVar a -> PermExpr tp ->
                       Binding tp (ValuePerm a) ->
                       SimplImpl (RNil :> a) (RNil :> a)
  -- ^ @SImpl_IntroExists x e p@ applies existential introduction:
  --
  -- > x:[e/z]p -o x:(exists z.p)

  SImpl_Cast :: ExprVar a -> ExprVar a -> ValuePerm a ->
                SimplImpl (RNil :> a :> a) (RNil :> a)
  -- ^ Cast a proof of @y:p@ to one of @x:p@ using @x:eq(y)@:
  --
  -- > x:eq(y) * y:p -o x:p

  SImpl_IntroEqRefl :: ExprVar a -> SimplImpl RNil (RNil :> a)
  -- ^ Introduce a proof that @x:eq(x)@:
  --
  -- > . -o x:eq(x)

  SImpl_CopyEq :: ExprVar a -> PermExpr a ->
                  SimplImpl (RNil :> a) (RNil :> a :> a)
  -- ^ Copy an equality proof on the top of the stack:
  --
  -- > x:eq(e) -o x:eq(e) * x:eq(e)

  SImpl_IntroConj :: ExprVar a -> SimplImpl RNil (RNil :> a)
  -- ^ Introduce an empty conjunction on @x@, i.e.:
  --
  -- > . -o x:true

  SImpl_ExtractConj :: ExprVar a -> [AtomicPerm a] -> Int ->
                       SimplImpl (RNil :> a) (RNil :> a :> a)
  -- ^ Extract the @i@th atomic permission out of a conjunct, putting it below
  -- that conjunct on the stack:
  --
  -- > x:(p0 * ... * p(n-1)) -o x:pi * x:(p0 * ... p(i-1) * p(i+1) ... * p(n-1))

  SImpl_CopyConj :: ExprVar a -> [AtomicPerm a] -> Int ->
                    SimplImpl (RNil :> a) (RNil :> a :> a)
  -- ^ Copy the @i@th atomic permission out of a conjunct, assuming it is
  -- copyable, putting it below that conjunct on the stack:
  --
  -- > x:(p0 * ... * p (n-1)) -o x:pi * x:(p0 * ... * p(n-1))

  SImpl_InsertConj :: ExprVar a -> AtomicPerm a -> [AtomicPerm a] -> Int ->
                      SimplImpl (RNil :> a :> a) (RNil :> a)
  -- ^ Insert an atomic permission below the top of the stack at the @i@th
  -- position in the conjunct on the top of the stack, where @i@ must be between
  -- @0@ and @n@ (the number of conjuncts), inclusive:
  --
  -- > x:p * x:(p0 * ... * p(n-1))
  -- >   -o x:(p0 * ... * p(i-1) * p * pi * ... * p(n-1))

  SImpl_ConstFunPerm ::
    args ~ CtxToRList cargs =>
    ExprVar (FunctionHandleType cargs ret) ->
    FnHandle cargs ret -> FunPerm ghosts (CtxToRList cargs) ret ->
    SimplImpl (RNil :> FunctionHandleType cargs ret)
    (RNil :> FunctionHandleType cargs ret)
  -- ^ Prove a function permission for a statically-known function (assuming
  -- that the given entry is in the current function environment):
  --
  -- > x:eq(handle) -o x:fun_perm

  SImpl_CastLLVMWord ::
    (1 <= w, KnownNat w) => ExprVar (LLVMPointerType w) ->
    PermExpr (BVType w) -> PermExpr (BVType w) ->
    SimplImpl (RNil :> LLVMPointerType w :> LLVMPointerType w)
    (RNil :> LLVMPointerType w)
  -- ^ Cast a proof of @x:eq(word(e1))@ to one of @x:eq(word(e2))@ using an
  -- equality permission @e1=e2@ on top of the stack:
  --
  -- > x:eq(word(e1)) * x:prop(e1=e2) -o x:eq(word(e2))

  SImpl_InvertLLVMOffsetEq ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> PermExpr (BVType w) ->
    ExprVar (LLVMPointerType w) ->
    SimplImpl (RNil :> LLVMPointerType w) (RNil :> LLVMPointerType w)
  -- ^ Invert an @x:eq(y+off)@ proof into a @y:eq(x-off)@ proof:
  --
  -- > x:eq(y+off) -o y:eq(x-off)

  SImpl_CastLLVMPtr ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> [AtomicPerm (LLVMPointerType w)] ->
    PermExpr (BVType w) -> ExprVar (LLVMPointerType w) ->
    SimplImpl (RNil :> LLVMPointerType w :> LLVMPointerType w)
    (RNil :> LLVMPointerType w)
  -- ^ Cast a conjunction @y:(p1 * ... * pn)@ of LLVM type on the top of the
  -- stack to @x:(p1 - off * ... * pn - off)@ using a proof of @x:eq(y+off)@
  -- just below it on the stack:
  --
  -- > x:eq(y+off) * y:(p1 * ... * pn) -o x:(p1 - off * ... * pn - off)

  SImpl_CastLLVMFree ::
    (1 <= w, KnownNat w) => ExprVar (LLVMPointerType w) ->
    PermExpr (BVType w) -> PermExpr (BVType w) ->
    SimplImpl (RNil :> LLVMPointerType w :> LLVMPointerType w)
    (RNil :> LLVMPointerType w)
  -- ^ Cast a proof of @x:free(e1)@ to one of @x:free(e2)@ using an equality
  -- permission @e1=e2@ on top of the stack:
  --
  -- > x:free(e1) * x:prop(e1=e2) -o x:free(e2)

  SImpl_CastLLVMFieldOffset ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> LLVMFieldPerm w -> PermExpr (BVType w) ->
    SimplImpl (RNil :> LLVMPointerType w :> LLVMPointerType w)
    (RNil :> LLVMPointerType w)
  -- ^ Cast the offset of a field permission from @off@ to @off'@ using an
  -- equality permission @off=off'@ on the top of the stack:
  --
  -- > x:ptr((rw,off) |-> p) * x:prop(off=off') -o x:ptr((rw,off') |-> p)

  SImpl_IntroLLVMFieldContents ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> ExprVar (LLVMPointerType w) ->
    LLVMFieldPerm w ->
    SimplImpl (RNil :> LLVMPointerType w :> LLVMPointerType w)
    (RNil :> LLVMPointerType w)
  -- ^ Combine proofs of @x:ptr((rw,off) |-> eq(y))@ and @y:p@ on the top of the
  -- permission stack into a proof of @x:ptr((rw,off) |-> p)@, where the
  -- supplied 'LLVMFieldPerm' gives the required output permission:
  --
  -- > x:ptr((rw,off) |-> eq(y)) * y:p -o x:ptr((rw,off) |-> p)

  SImpl_LLVMFieldLifetimeCurrent ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> LLVMFieldPerm w ->
    ExprVar LifetimeType -> PermExpr LifetimeType ->
    SimplImpl (RNil :> LLVMPointerType w :> LifetimeType)
    (RNil :> LLVMPointerType w)
  -- ^ Change the lifetime of a field permission to one during which the
  -- existing lifetime is current:
  --
  -- > x:[l1]ptr((rw,off) |-> p) * l1:[l2]lcurrent -o [l2]x:ptr((rw,off) |-> p)

  SImpl_LLVMFieldLifetimeAlways ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> LLVMFieldPerm w -> PermExpr LifetimeType ->
    SimplImpl (RNil :> LLVMPointerType w) (RNil :> LLVMPointerType w)
  -- ^ Change the lifetime of a field permission whose existing lifetime is
  -- always:
  --
  -- > x:[always]ptr((rw,off) |-> p) -o [l]x:ptr((rw,off) |-> p)

  SImpl_DemoteLLVMFieldWrite ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> LLVMFieldPerm w ->
    SimplImpl (RNil :> LLVMPointerType w) (RNil :> LLVMPointerType w)
  -- ^ Demote an LLVM field permission from write to read:
  --
  -- > x:[ls]ptr((W,off) |-> p) -o [ls]x:ptr((R,off) |-> p)

  SImpl_LLVMArrayCopy ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> LLVMArrayPerm w -> BVRange w ->
    SimplImpl (RNil :> LLVMPointerType w :> LLVMPointerType w)
    (RNil :> LLVMPointerType w :> LLVMPointerType w)
  -- ^ Copy a range of an array permission given by an offset and length,
  -- assuming that all fields of the array are copyable, using a range subset
  -- permission on the top of the stack:
  --
  -- > x:array(off,<len,*stride,fps,bs) * x:prop([off',len') <= [off,len))
  -- >   -o x:array(off',<len',*stride,fps,bs)
  -- >      * x:array(off,<len,*stride,fps,bs)

  SImpl_LLVMArrayBorrow ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> LLVMArrayPerm w -> BVRange w ->
    SimplImpl (RNil :> LLVMPointerType w :> LLVMPointerType w)
    (RNil :> LLVMPointerType w :> LLVMPointerType w)
  -- ^ Borrow a range of an array permission at the given offset and length,
  -- adding a borrow to the original array permission and putting the new array
  -- permission just below the top of the stack. Requires a conjunction of
  -- propositional permissions stating that the given offset and length form a
  -- subset of the original offset and length and do not overlap with any of the
  -- existing borrows.
  --
  -- > x:array(off,<len,*stride,fps,bs)
  -- > * x:(prop([off',len') <= [off,len)) * disjoint(bs,[off',len')))
  -- >   -o x:array(off',<len',*stride,fps,[])
  -- >      * x:array(off,<len,*stride,fps,[off',len'):bs)

  SImpl_LLVMArrayReturn ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> LLVMArrayPerm w -> BVRange w ->
    SimplImpl (RNil :> LLVMPointerType w :> LLVMPointerType w)
    (RNil :> LLVMPointerType w)
  -- ^ Return a borrowed range of an array permission, undoing a borrow:
  --
  -- > x:array(off',<len',*stride,fps,[])
  -- > * x:array(off,<len,*stride,fps,[off',len'):bs)
  -- >   -o x:array(off,<len,*stride,fps,bs)

  SImpl_LLVMArrayIndexCopy ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> LLVMArrayPerm w ->
    PermExpr (BVType w) -> Int ->
    SimplImpl (RNil :> LLVMPointerType w :> LLVMPointerType w)
    (RNil :> LLVMPointerType w :> LLVMPointerType w)
  -- ^ Copy out the @j@th field permission of the @i@th element of an array
  -- permission, assuming that the @j@th field permission is copyable, where @j@
  -- is a static 'Int' and @i@ is an expression. Requires a proposition
  -- permission on the top of the stack stating that @i@ is in the range of the
  -- array and that the specified field permission does not overlap with any of
  -- the existing borrows:
  --
  -- > x:array(off,<len,*stride,fps,bs)
  -- > * x:(prop(i \in [off,len)) * disjoint(bs,i*stride+j))
  -- >   -o x:(fp_j + off+i*stride+j) * x:array(off,<len,*stride,fps,bs)

  SImpl_LLVMArrayIndexBorrow ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> LLVMArrayPerm w ->
    PermExpr (BVType w) -> Int ->
    SimplImpl (RNil :> LLVMPointerType w :> LLVMPointerType w)
    (RNil :> LLVMPointerType w :> LLVMPointerType w)
  -- ^ Borrow the @j@th field permission of the @i@th element of an array
  -- permission, where @j@ is a static 'Int' and @i@ is an expression. Requires
  -- a proposition permission on the top of the stack stating that @i@ is in the
  -- range of the array and that the specified field permission does not overlap
  -- with any of the existing borrows, and adds a borrow of the given field:
  --
  -- > x:array(off,<len,*stride,fps,bs)
  -- > * x:(prop(i \in [off,len)) * disjoint(bs,i*stride+j))
  -- >   -o x:(fp_j + offset i*stride)
  -- >      * x:array(off,<len,*stride,fps,(i*stride+j):bs)

  SImpl_LLVMArrayIndexReturn ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> LLVMArrayPerm w ->
    PermExpr (BVType w) -> Int ->
    SimplImpl (RNil :> LLVMPointerType w :> LLVMPointerType w)
    (RNil :> LLVMPointerType w)
  -- ^ Return the @j@th field permission of the @i@th element of an array
  -- permission, where @j@ is a static 'Int' and @i@ is an expression, undoing a
  -- borrow:
  --
  -- > x:(fp_j + offset+i*stride+j)
  -- > * x:array(off,<len,*stride,fps,(i*stride+j):bs)
  -- >   -o x:array(off,<len,*stride,fps,bs)

  SImpl_LLVMArrayContents ::
    (1 <= w, KnownNat w) =>
    ExprVar (LLVMPointerType w) -> LLVMArrayPerm w -> Int -> LLVMFieldPerm w ->
    PermImpl ((:~:) (RNil :> LLVMPointerType w)) (RNil :> LLVMPointerType w) ->
    SimplImpl (RNil :> LLVMPointerType w) (RNil :> LLVMPointerType w)
  -- ^ Apply an implication to the @i@th field of an array permission:
  --
  -- > y:fpi -o y:fpi'
  -- > ------------------------------------------------
  -- > x:array(off,<len,*stride,(fp1, ..., fpn),bs) -o
  -- >   x:array(off,<len,*stride,
  -- >           (fp1, ..., fp(i-1), fpi', fp(i+1), ..., fpn),bs)

  SImpl_SplitLifetime ::
    KnownRepr TypeRepr a => ExprVar a -> ValuePerm a ->
    ExprVar LifetimeType -> PermExpr PermListType ->
    SimplImpl (RNil :> a :> LifetimeType)
    (RNil :> a :> LifetimeType)
  -- ^ Save a permission for later by splitting it into part that is in the
  -- current lifetime and part that is saved in the lifetime for later:
  --
  -- > x:p * l:lowned(ps) -o x:(inLifetime l p) * l:lowned(x:p,ps)

  SImpl_LCurrentRefl :: ExprVar LifetimeType ->
                        SimplImpl RNil (RNil :> LifetimeType)
  -- ^ Reflexivity for @lcurrent@ proofs:
  --
  -- > . -o l:lcurrent(l)

  SImpl_LCurrentTrans :: ExprVar LifetimeType -> ExprVar LifetimeType ->
                         PermExpr LifetimeType ->
                         SimplImpl (RNil :> LifetimeType :> LifetimeType)
                         (RNil :> LifetimeType)
  -- ^ Transitively combine @lcurrent@ proofs:
  --
  -- > l1:lcurrent(l2) * l2:lcurrent(l3) -o l1:lcurrent(l3)

  SImpl_FoldMu :: ExprVar a -> Binding (ValuePerm a) (ValuePerm a) ->
                  SimplImpl (RNil :> a) (RNil :> a)
  -- ^ Fold a mu permission:
  --
  -- > x:[mu X.p / X]p -o x:(mu X.p)

  SImpl_UnfoldMu :: ExprVar a -> Binding (ValuePerm a) (ValuePerm a) ->
                    SimplImpl (RNil :> a) (RNil :> a)
  -- ^ Unfold a mu permission:
  --
  -- > x:(mu X.p) -o x:[mu X.p / X]p

  SImpl_Mu ::
    ExprVar a -> Binding (ValuePerm a) (ValuePerm a) ->
    Binding (ValuePerm a) (ValuePerm a) ->
    PermImpl ((:~:) (RNil :> a)) (RNil :> a) ->
    SimplImpl (RNil :> a) (RNil :> a)
  -- ^ Apply an implication to the body of a least fixed-point permission:
  --
  -- > y:p1 -o y:p2
  -- > ----------------------
  -- > x:mu X.p1 -o x:mu X.p2


-- | A single step of permission implication. These can have multiple,
-- disjunctive conclusions, each of which can bind some number of variables, and
-- can also move permissions between the primary permissions for each variable
-- and the permission stack. The general form is:
--
-- > x1::Px1 * ... * xl::Pl * P1 * ... * Pn
-- >   -o (zs1 . x1::Px1_1 * ... * xl::Pxl_1 * P1_1 * ... * P1_k1) \/
-- >      ... \/ (zsm . x1::Px1_m * ... * xl::Pxl_m * Pm_1 * ... * Pm_km)
--
-- where @zsi@ is a list of permission variables bound in the permissions @Pi_j@
-- and @xi::Pxi@ denotes the primary permission for variable @xi@. In the
-- comments below, we often omit the primary variable permissions when they do
-- not change. The types of @P1@ through @Pn@ are given by the first type
-- argument @ps_in@ of this type, while those of the @zsi@ and the @Pi_j@
-- permissions are given by the @ps_outs@ argument. The latter is an 'RList' of
-- the form
--
-- > RNil :> (bs1, ps1) :> ... :> (bsm, psm)
--
-- where each @bsi@ is itself an 'RList' of the types of the bound variables in
-- @zsi@ and @psi@ is an 'RList' of the types of @Pi_1@ through @Pi_km@.
data PermImpl1 ps_in ps_outs where
  Impl1_Fail :: PermImpl1 ps RNil
  -- ^ Failure of a permission implication, which is a proof of 0 disjuncts:
  --
  -- > ps -o .

  Impl1_Catch :: PermImpl1 ps (RNil :> '(RNil, ps) :> '(RNil, ps))
  -- ^ Catch any failure in the first branch by calling the second, passing the
  -- same input permissions to both branches:
  --
  -- > ps -o ps \/ ps

  Impl1_Push :: ExprVar a -> ValuePerm a ->
                PermImpl1 ps (RNil :> '(RNil, ps :> a))
  -- ^ Push the primary permission for variable @x@ onto the stack:
  --
  -- > x::P * ps -o x::true * ps * x:P

  Impl1_Pop :: ExprVar a -> ValuePerm a ->
               PermImpl1 (ps :> a) (RNil :> '(RNil, ps))
  -- ^ Pop the a permission for variable @x@ back to the primary permission for
  -- @x@, assuming the latter is the trivial permission @true@:
  --
  -- > x::true * ps * x:P -o x::P * ps

  Impl1_ElimOr :: ExprVar a -> ValuePerm a -> ValuePerm a ->
                  PermImpl1 (ps :> a)
                  (RNil :> '(RNil, ps :> a) :> '(RNil, ps :> a))
  -- ^ Eliminate a disjunction on the top of the stack:
  --
  -- > ps * x:(p1 \/ p2) -o (ps * x:p1) \/ (ps * x:p2)

  Impl1_ElimExists :: KnownRepr TypeRepr tp => ExprVar a ->
                      Binding tp (ValuePerm a) ->
                      PermImpl1 (ps :> a) (RNil :> '(RNil :> tp, ps :> a))
  -- ^ Eliminate an existential on the top of the stack:
  --
  -- > ps * x:(exists z.p) -o z. ps * x:p

  Impl1_Simpl :: SimplImpl ps_in ps_out -> Proxy ps ->
                 PermImpl1 (ps :++: ps_in) (RNil :> '(RNil, ps :++: ps_out))
  -- ^ Apply a 'SimplImpl'

  Impl1_ElimLLVMFieldContents ::
    (1 <= w, KnownNat w) => ExprVar (LLVMPointerType w) -> LLVMFieldPerm w ->
    PermImpl1 (ps :> LLVMPointerType w)
    (RNil :> '(RNil :> LLVMPointerType w,
               ps :> LLVMPointerType w :> LLVMPointerType w))
  -- ^ Eliminate the contents of an LLVM field permission, binding a new
  -- variable to hold those permissions and changing the contents of the field
  -- permission to an equals permision for that variable:
  --
  -- > x:ptr((rw,off) -> p) -o y. x:ptr((rw,off) -> eq(y)) * y:p

  Impl1_TryProveBVProp ::
    (1 <= w, KnownNat w) => ExprVar (LLVMPointerType w) -> BVProp w ->
    PermImpl1 ps (RNil :> '(RNil, ps :> LLVMPointerType w))
  -- ^ Try to prove a bitvector proposition, or fail (as in the 'Impl1_Fail'
  -- rule) if this is not possible:
  --
  -- > . -o prop(p)


-- | A @'PermImpl' r ps@ is a proof tree of the judgment
--
-- > Gamma | Pl * P |- (Gamma1 | Pl1 * P1) \/ ... \/ (Gamman | Pln * Pn)
--
-- that contains an element of type @r@ at each leaf of the proof tree. Each
-- disjunct on the right of the judgment corresponds to a different leaf in the
-- tree, while each @Gammai@ denotes the variables that are bound on the path
-- from the root to that leaf. The @ps@ argument captures the form of the
-- "distinguished" left-hand side permissions @Pl@.
--
-- FIXME: explain that @Pl@ is like a stack, and that intro rules apply to the
-- top of the stack
--
-- FIXME: it would be nice to have PermImpl r ps_out ps_in, where ps_out is
-- guaranteed to be the stack shape at any Impl_Done, but this would make our
-- generalized monad below more complicated...
data PermImpl r ps where
  PermImpl_Done :: r ps -> PermImpl r ps
  PermImpl_Step :: PermImpl1 ps_in ps_outs ->
                   MbPermImpls r ps_outs ->
                   PermImpl r ps_in

-- | Helper type for 'PermImpl', that defines a collection of permission
-- implications, one for each element of the @bs_pss@ type argument. Each of
-- these elements are of the form @(bs,ps)@, where @ps@ determines the input
-- permissions for that implication tree and @bs@ specifies an existential
-- contexts of bound variables for that implication tree.
data MbPermImpls r bs_pss where
  MbPermImpls_Nil :: MbPermImpls r RNil
  MbPermImpls_Cons :: MbPermImpls r bs_pss -> Mb bs (PermImpl r ps) ->
                      MbPermImpls r (bs_pss :> '(bs,ps))

-- type IsLLVMPointerTypeList w ps = MapRList ((:~:) (LLVMPointerType w)) ps



$(mkNuMatching [t| forall ps_in ps_out. SimplImpl ps_in ps_out |])
$(mkNuMatching [t| forall ps_in ps_outs. PermImpl1 ps_in ps_outs |])
$(mkNuMatching [t| forall r bs_pss. NuMatchingAny1 r => MbPermImpls r bs_pss |])
$(mkNuMatching [t| forall r ps. NuMatchingAny1 r => PermImpl r ps |])


-- | Compute the input permissions of a 'SimplImpl' implication
simplImplIn :: SimplImpl ps_in ps_out -> DistPerms ps_in
simplImplIn (SImpl_Drop x p) = distPerms1 x p
simplImplIn (SImpl_Swap x p1 y p2) = distPerms2 x p1 y p2
simplImplIn (SImpl_IntroOrL x p1 p2) = distPerms1 x p1
simplImplIn (SImpl_IntroOrR x p1 p2) = distPerms1 x p2
simplImplIn (SImpl_IntroExists x e p) =
  distPerms1 x (subst (singletonSubst e) p)
simplImplIn (SImpl_Cast x y p) = distPerms2 x (ValPerm_Eq $ PExpr_Var y) y p
simplImplIn (SImpl_IntroEqRefl x) = DistPermsNil
simplImplIn (SImpl_CopyEq x e) = distPerms1 x (ValPerm_Eq e)
simplImplIn (SImpl_IntroConj x) = DistPermsNil
simplImplIn (SImpl_ExtractConj x ps i) = distPerms1 x (ValPerm_Conj ps)
simplImplIn (SImpl_CopyConj x ps i) = distPerms1 x (ValPerm_Conj ps)
simplImplIn (SImpl_InsertConj x p ps i) =
  distPerms2 x (ValPerm_Conj [p]) x (ValPerm_Conj ps)
simplImplIn (SImpl_ConstFunPerm x h _) =
  distPerms1 x (ValPerm_Eq $ PExpr_Fun h)
simplImplIn (SImpl_CastLLVMWord x e1 e2) =
  distPerms2 x (ValPerm_Eq $ PExpr_LLVMWord e1)
  x (ValPerm_Conj [Perm_BVProp $ BVProp_Eq e1 e2])
simplImplIn (SImpl_InvertLLVMOffsetEq x off y) =
  distPerms1 x $ ValPerm_Eq $ PExpr_LLVMOffset y off
simplImplIn (SImpl_CastLLVMPtr y ps off x) =
  distPerms2 x (ValPerm_Eq $ PExpr_LLVMOffset y off) y (ValPerm_Conj ps)
simplImplIn (SImpl_CastLLVMFree x e1 e2) =
  distPerms2 x (ValPerm_Conj [Perm_LLVMFree e1])
  x (ValPerm_Conj [Perm_BVProp $ BVProp_Eq e1 e2])
simplImplIn (SImpl_CastLLVMFieldOffset x fld off') =
  distPerms2 x (ValPerm_Conj [Perm_LLVMField fld])
  x (ValPerm_Conj [Perm_BVProp $ BVProp_Eq (llvmFieldOffset fld) off'])
simplImplIn (SImpl_IntroLLVMFieldContents x y fld) =
  distPerms2 x (ValPerm_Conj [Perm_LLVMField $
                              fld { llvmFieldContents =
                                    ValPerm_Eq (PExpr_Var y)}])
  y (llvmFieldContents fld)
simplImplIn (SImpl_LLVMFieldLifetimeCurrent x fld l1 l2) =
  distPerms2 x (ValPerm_Conj [Perm_LLVMField fld])
  l1 (ValPerm_Conj [Perm_LCurrent l2])
simplImplIn (SImpl_LLVMFieldLifetimeAlways x fld l) =
  if llvmFieldLifetime fld == PExpr_Always then
    distPerms1 x (ValPerm_Conj [Perm_LLVMField fld])
  else
    error "simplImplIn: SImpl_LLVMFieldLifetimeAlways: input lifetime is not always"
simplImplIn (SImpl_DemoteLLVMFieldWrite x fld) =
  distPerms1 x (ValPerm_Conj [Perm_LLVMField $
                              fld { llvmFieldRW = Write }])
simplImplIn (SImpl_LLVMArrayCopy x ap rng) =
  if atomicPermIsCopyable (Perm_LLVMArray ap) then
    distPerms2 x (ValPerm_Conj [Perm_LLVMArray ap])
    x (ValPerm_Conj [Perm_BVProp $ BVProp_RangeSubset rng $ llvmArrayRange ap])
  else
    error "simplImplIn: SImpl_LLVMArrayCopy: array permission not copyable"

simplImplIn (SImpl_LLVMArrayBorrow x ap rng) =
  distPerms2 x (ValPerm_Conj [Perm_LLVMArray ap])
  x (ValPerm_Conj (Perm_BVProp (BVProp_RangeSubset rng (llvmArrayRange ap))
                   : map Perm_BVProp (llvmArrayBorrowsDisjoint ap rng)))

simplImplIn (SImpl_LLVMArrayReturn x ap rng) =
  if llvmArrayRangeIsBorrowed ap rng then
    distPerms2 x
    (ValPerm_Conj [Perm_LLVMArray $
                   ap { llvmArrayOffset = bvRangeOffset rng,
                        llvmArrayLen = bvRangeLength rng,
                        llvmArrayBorrows = [] }])
    x (ValPerm_Conj [Perm_LLVMArray ap])
  else
    error "simplImplIn: SImpl_LLVMArrayReturn: range not being borrowed"

simplImplIn (SImpl_LLVMArrayIndexCopy x ap i j) =
  if atomicPermIsCopyable (Perm_LLVMField (llvmArrayFields ap !! j)) then
    distPerms2 x (ValPerm_Conj [Perm_LLVMArray ap])
    x (ValPerm_Conj (Perm_BVProp (BVProp_InRange
                                  (llvmArrayIndex ap i j) (llvmArrayRange ap))
                     :
                     map Perm_BVProp
                     (llvmArrayBorrowsDisjoint ap
                      (bvRangeOfIndex (llvmArrayIndex ap i j)))))
  else
    error "simplImplIn: SImpl_LLVMArrayIndexCopy: field is not copyable"

simplImplIn (SImpl_LLVMArrayIndexBorrow x ap i j) =
  distPerms2 x (ValPerm_Conj [Perm_LLVMArray ap])
  x (ValPerm_Conj (Perm_BVProp (BVProp_InRange
                                (llvmArrayIndex ap i j) (llvmArrayRange ap))
                   : (map Perm_BVProp $
                      llvmArrayBorrowsDisjoint ap (bvRangeOfIndex
                                                   (llvmArrayIndex ap i j)))))

simplImplIn (SImpl_LLVMArrayIndexReturn x ap i j) =
  if llvmArrayRangeIsBorrowed ap (bvRangeOfIndex
                                  (llvmArrayIndex ap i j)) then
    distPerms2 x (ValPerm_Conj [Perm_LLVMField $
                                llvmArrayFieldWithOffset ap i j])
    x (ValPerm_Conj [Perm_LLVMArray ap])
  else
    error "simplImplIn: SImpl_LLVMArrayIndexReturn: index not being borrowed"

simplImplIn (SImpl_LLVMArrayContents x ap _ _ _) =
  distPerms1 x (ValPerm_Conj [Perm_LLVMArray ap])

simplImplIn (SImpl_SplitLifetime x p l ps) =
  distPerms2 x p l (ValPerm_Conj [Perm_LOwned ps])
simplImplIn (SImpl_LCurrentRefl _) = DistPermsNil
simplImplIn (SImpl_LCurrentTrans l1 l2 l3) =
  distPerms2 l1 (ValPerm_Conj [Perm_LCurrent $ PExpr_Var l2])
  l2 (ValPerm_Conj [Perm_LCurrent l3])
simplImplIn (SImpl_FoldMu x p) =
  distPerms1 x (substValPerm (ValPerm_Mu p) p)
simplImplIn (SImpl_UnfoldMu x p) = distPerms1 x (ValPerm_Mu p)
simplImplIn (SImpl_Mu x p1 _ _) = distPerms1 x (ValPerm_Mu p1)


-- | Compute the output permissions of a 'SimplImpl' implication
simplImplOut :: SimplImpl ps_in ps_out -> DistPerms ps_out
simplImplOut (SImpl_Drop x p) = DistPermsNil
simplImplOut (SImpl_Swap x p1 y p2) = distPerms2 y p2 x p1
simplImplOut (SImpl_IntroOrL x p1 p2) = distPerms1 x (ValPerm_Or p1 p2)
simplImplOut (SImpl_IntroOrR x p1 p2) = distPerms1 x (ValPerm_Or p1 p2)
simplImplOut (SImpl_IntroExists x _ p) = distPerms1 x (ValPerm_Exists p)
simplImplOut (SImpl_Cast x _ p) = distPerms1 x p
simplImplOut (SImpl_IntroEqRefl x) = distPerms1 x (ValPerm_Eq $ PExpr_Var x)
simplImplOut (SImpl_CopyEq x e) = distPerms2 x (ValPerm_Eq e) x (ValPerm_Eq e)
simplImplOut (SImpl_IntroConj x) = distPerms1 x ValPerm_True
simplImplOut (SImpl_ExtractConj x ps i) =
  distPerms2 x (ValPerm_Conj [ps !! i])
  x (ValPerm_Conj (take i ps ++ drop (i+1) ps))
simplImplOut (SImpl_CopyConj x ps i) =
  if atomicPermIsCopyable (ps !! i) then
    distPerms2 x (ValPerm_Conj [ps !! i]) x (ValPerm_Conj ps)
  else
    error "simplImplOut: SImpl_CopyConj: permission not copyable"
simplImplOut (SImpl_InsertConj x p ps i) =
  distPerms1 x (ValPerm_Conj (take i ps ++ p : drop i ps))
simplImplOut (SImpl_ConstFunPerm x _ fun_perm) =
  distPerms1 x (ValPerm_Conj1 $ Perm_Fun fun_perm)
simplImplOut (SImpl_CastLLVMWord x _ e2) =
  distPerms1 x (ValPerm_Eq $ PExpr_LLVMWord e2)
simplImplOut (SImpl_InvertLLVMOffsetEq x off y) =
  distPerms1 y $ ValPerm_Eq $ PExpr_LLVMOffset x $ bvNegate off
simplImplOut (SImpl_CastLLVMPtr _ ps off x) =
  distPerms1 x (ValPerm_Conj $ map (offsetLLVMAtomicPerm $ bvNegate off) ps)
simplImplOut (SImpl_CastLLVMFree x _ e2) =
  distPerms1 x (ValPerm_Conj [Perm_LLVMFree e2])
simplImplOut (SImpl_CastLLVMFieldOffset x fld off') =
  distPerms1 x (ValPerm_Conj [Perm_LLVMField $ fld { llvmFieldOffset = off' }])
simplImplOut (SImpl_IntroLLVMFieldContents x _ fld) =
  distPerms1 x (ValPerm_Conj [Perm_LLVMField fld])
simplImplOut (SImpl_LLVMFieldLifetimeCurrent x fld l1 l2) =
  distPerms1 x (ValPerm_Conj [Perm_LLVMField fld { llvmFieldLifetime = l2 }])
simplImplOut (SImpl_LLVMFieldLifetimeAlways x fld l) =
  distPerms1 x (ValPerm_Conj [Perm_LLVMField $ fld { llvmFieldLifetime = l }])
simplImplOut (SImpl_DemoteLLVMFieldWrite x fld) =
  distPerms1 x (ValPerm_Conj [Perm_LLVMField $
                              fld { llvmFieldRW = Read }])
simplImplOut (SImpl_LLVMArrayCopy x ap rng) =
  if atomicPermIsCopyable (Perm_LLVMArray ap) then
    distPerms2 x (ValPerm_Conj [Perm_LLVMArray $
                                ap { llvmArrayOffset = bvRangeOffset rng,
                                     llvmArrayLen = bvRangeLength rng }])
    x (ValPerm_Conj [Perm_LLVMArray ap])
  else
    error "simplImplOut: SImpl_LLVMArrayCopy: array permission not copyable"

simplImplOut (SImpl_LLVMArrayBorrow x ap rng) =
  distPerms2 x (ValPerm_Conj [Perm_LLVMArray $
                              ap { llvmArrayOffset = bvRangeOffset rng,
                                   llvmArrayLen = bvRangeLength rng }])
  x (ValPerm_Conj [Perm_LLVMArray $
                              ap { llvmArrayBorrows =
                                     RangeBorrow rng : llvmArrayBorrows ap }])

simplImplOut (SImpl_LLVMArrayReturn x ap rng) =
  if llvmArrayRangeIsBorrowed ap rng then
    distPerms1 x
    (ValPerm_Conj [Perm_LLVMArray $
                   ap { llvmArrayBorrows =
                          delete (RangeBorrow rng) (llvmArrayBorrows ap) }])
  else
    error "simplImplOut: SImpl_LLVMArrayReturn: range not being borrowed"

simplImplOut (SImpl_LLVMArrayIndexCopy x ap i j) =
  if atomicPermIsCopyable (Perm_LLVMField (llvmArrayFields ap !! j)) then
    distPerms2 x (ValPerm_Conj [Perm_LLVMField $
                                llvmArrayFieldWithOffset ap i j])
    x (ValPerm_Conj [Perm_LLVMArray ap])
  else
    error "simplImplOut: SImpl_LLVMArrayIndexCopy: field is not copyable"

simplImplOut (SImpl_LLVMArrayIndexBorrow x ap i j) =
  distPerms2 x (ValPerm_Conj [Perm_LLVMField $
                              llvmArrayFieldWithOffset ap i j])
  x (ValPerm_Conj [Perm_LLVMArray $
                   ap { llvmArrayBorrows =
                          FieldBorrow i (toInteger j) Nothing
                          : llvmArrayBorrows ap }])

simplImplOut (SImpl_LLVMArrayIndexReturn x ap i j) =
  let rng = bvRangeOfIndex (llvmArrayIndex ap i j) in
  if llvmArrayRangeIsBorrowed ap rng then
    distPerms1 x
    (ValPerm_Conj [Perm_LLVMArray $ llvmArrayRemIndexBorrow i j ap])
  else
    error "simplImplOut: SImpl_LLVMArrayIndexReturn: index not being borrowed"

simplImplOut (SImpl_LLVMArrayContents x ap i fp _) =
  distPerms1 x (ValPerm_Conj [Perm_LLVMArray $
                              ap { llvmArrayFields =
                                     take i (llvmArrayFields ap) ++
                                     fp : drop (i+1) (llvmArrayFields ap)}])

simplImplOut (SImpl_SplitLifetime x p l ps) =
  distPerms2 x (inLifetime (PExpr_Var l) p)
  l (ValPerm_Conj [Perm_LOwned (PExpr_PermListCons (PExpr_Var x) p ps)])
simplImplOut (SImpl_LCurrentRefl l) =
  distPerms1 l (ValPerm_Conj1 $ Perm_LCurrent $ PExpr_Var l)
simplImplOut (SImpl_LCurrentTrans l1 _ l3) =
  distPerms1 l1 (ValPerm_Conj [Perm_LCurrent l3])
simplImplOut (SImpl_FoldMu x p) = distPerms1 x (ValPerm_Mu p)
simplImplOut (SImpl_UnfoldMu x p) =
  distPerms1 x (substValPerm (ValPerm_Mu p) p)
simplImplOut (SImpl_Mu x _ p2 _) = distPerms1 x (ValPerm_Mu p2)


-- | Apply a 'SimplImpl' implication to the permissions on the top of a
-- permission set stack, checking that they equal the 'simplImplIn' of the
-- 'SimplImpl' and then replacing them with its 'simplImplOut'
applySimplImpl :: Proxy ps -> SimplImpl ps_in ps_out ->
                  PermSet (ps :++: ps_in) -> PermSet (ps :++: ps_out)
applySimplImpl prx simpl =
  modifyDistPerms $ \all_ps ->
  let (ps, ps_in) =
        splitDistPerms prx (distPermsToProxies $ simplImplIn simpl) all_ps in
  if ps_in == simplImplIn simpl then
    appendDistPerms ps (simplImplOut simpl)
  else
    error "applySimplImpl: input permissions are not as expected!"

-- | A sequence of permission sets inside name-bindings
data MbPermSets bs_pss where
  MbPermSets_Nil :: MbPermSets RNil
  MbPermSets_Cons :: MbPermSets bs_pss -> Mb bs (PermSet ps) ->
                     MbPermSets (bs_pss :> '(bs,ps))

-- | Helper for building a one-element 'MbPermSets' sequence
mbPermSets1 :: Mb bs (PermSet ps) -> MbPermSets (RNil :> '(bs,ps))
mbPermSets1 = MbPermSets_Cons MbPermSets_Nil

-- | Helper for building a two-element 'MbPermSets' sequence
mbPermSets2 :: Mb bs1 (PermSet ps1) -> Mb bs2 (PermSet ps2) ->
               MbPermSets (RNil :> '(bs1,ps1) :> '(bs2,ps2))
mbPermSets2 ps1 ps2 = MbPermSets_Cons (MbPermSets_Cons MbPermSets_Nil ps1) ps2

-- | Apply a single permission implication step to a permission set
applyImpl1 :: PermImpl1 ps_in ps_outs -> PermSet ps_in -> MbPermSets ps_outs
applyImpl1 Impl1_Fail _ = MbPermSets_Nil
applyImpl1 Impl1_Catch ps = mbPermSets2 (emptyMb ps) (emptyMb ps)
applyImpl1 (Impl1_Push x p) ps =
  if ps ^. varPerm x == p then
    mbPermSets1 $ emptyMb $ pushPerm x p $ set (varPerm x) ValPerm_True ps
  else
    error "applyImpl1: Impl1_Push: unexpected permission"
applyImpl1 (Impl1_Pop x p) ps =
  if ps ^. topDistPerm x == p then
    mbPermSets1 $ emptyMb $ fst $ popPerm x $ set (varPerm x) p ps
  else
    error "applyImpl1: Impl1_Pop: unexpected permission"
applyImpl1 (Impl1_ElimOr x p1 p2) ps =
  if ps ^. topDistPerm x == ValPerm_Or p1 p2 then
    mbPermSets2 (emptyMb $ set (topDistPerm x) p1 ps)
    (emptyMb $ set (topDistPerm x) p2 ps)
  else
    error "applyImpl1: Impl1_ElimOr: unexpected permission"
applyImpl1 (Impl1_ElimExists x p_body) ps =
  if ps ^. topDistPerm x == ValPerm_Exists p_body then
    mbPermSets1 (fmap (\p -> set (topDistPerm x) p ps) p_body)
  else
    error "applyImpl1: Impl1_ElimExists: unexpected permission"
applyImpl1 (Impl1_Simpl simpl prx) ps =
  mbPermSets1 $ emptyMb $ applySimplImpl prx simpl ps
applyImpl1 (Impl1_ElimLLVMFieldContents x fp) ps =
  if ps ^. topDistPerm x == ValPerm_Conj [Perm_LLVMField fp] then
    (mbPermSets1 $ nu $ \y ->
      pushPerm y (llvmFieldContents fp) $
      set (topDistPerm x) (ValPerm_Conj [Perm_LLVMField $
                                         fp { llvmFieldContents =
                                              ValPerm_Eq (PExpr_Var y) }])
      ps)
  else
    error "applyImpl1: Impl1_ElimLLVMFieldContents: unexpected permission"
applyImpl1 (Impl1_TryProveBVProp x prop) ps =
  mbPermSets1 $ emptyMb $
  pushPerm x (ValPerm_Conj [Perm_BVProp prop]) ps


----------------------------------------------------------------------
-- * Generalized Monads
----------------------------------------------------------------------

-- | A generalized monad has additional "input" and "output" types, that
-- sequence together "backwards" through the generalized bind operation. Mostly
-- this is to support 'GenContM', below.
--
-- Note that a generalized monad @m@ should always be a standard monad when the
-- input and output types are the same; i.e., @m r r@ should always be a
-- monad. I do not know a nice way to encode this in Haskell, however...
class GenMonad (m :: k -> k -> Kind.* -> Kind.*) where
  -- | Generalized return
  greturn :: a -> m r r a
  -- | Generalized bind, that passes the output of @f@ to the input of @m@
  (>>>=) :: m r2 r3 a -> (a -> m r1 r2 b) -> m r1 r3 b

infixl 1 >>>=
infixl 1 >>>

(>>>) :: GenMonad m => m r2 r3 () -> m r1 r2 a -> m r1 r3 a
m1 >>> m2 = m1 >>>= \() -> m2

class GenMonadT (t :: (k1 -> k1 -> Kind.* -> Kind.*) ->
                 k2 -> k2 -> Kind.* -> Kind.*) p1 p2 q1 q2 |
  t q1 q2 -> p1 p2 , t q1 p2 -> q2 p1 , t p1 q2 -> p2 q1 where
  glift :: GenMonad m => m p1 p2 a -> t m q1 q2 a


-- | The generalized continuation transformer, which can have different types
-- for the input vs output continuations
newtype GenContM rin rout a =
  GenContM { runGenContM :: (a -> rin) -> rout }

liftGenContM :: Monad m => m a -> GenContM (m b) (m b) a
liftGenContM m = GenContM $ \k -> m >>= k

instance Functor (GenContM r r) where
  fmap f m = m >>= return . f

instance Applicative (GenContM r r) where
  pure = return
  (<*>) = ap

instance Monad (GenContM r r) where
  return x = GenContM $ \k -> k x
  GenContM m >>= f = GenContM $ \k -> m $ \a -> runGenContM (f a) k

instance GenMonad GenContM where
  greturn x = GenContM $ \k -> k x
  (GenContM m) >>>= f = GenContM $ \k -> m $ \a -> runGenContM (f a) k

{-
-- | Change the return type constructor @r@ by mapping the new input type to the
-- old and mapping the old output type to the new
withAltContM :: (r2 pin2 -> r1 pin1) -> (r1 pout1 -> r2 pout2) ->
                GenContM r1 pin1 pout1 a -> GenContM r2 pin2 pout2 a
withAltContM f_in f_out (GenContM m) =
  GenContM $ \k -> f_out (m (f_in . k))
-}

-- | Typeclass for generalized monads that allow a pure capture of the CC
class GenMonad m => GenMonadCaptureCC rin rout m p1 p2 |
  m p1 -> rin , m p2 -> rout , m p1 rout -> p2 , m p2 rin -> p1 where
  gcaptureCC :: ((a -> rin) -> rout) -> m p1 p2 a

instance GenMonadCaptureCC r1 r2 GenContM r1 r2 where
  gcaptureCC f = GenContM f

instance GenMonadCaptureCC rin rout m q1 q2 =>
         GenMonadCaptureCC rin rout (GenStateT m) '(s,q1) '(s,q2) where
  gcaptureCC f = glift $ gcaptureCC f

gmapRet :: GenMonadCaptureCC rin rout m p1 p2 => (rin -> rout) -> m p1 p2 ()
gmapRet f_ret =
  gcaptureCC $ \k -> f_ret $ k ()

-- | Name-binding in the generalized continuation monad (FIXME: explain)
gopenBinding :: GenMonadCaptureCC rin rout m p1 p2 =>
                (Mb ctx rin -> rout) -> Mb ctx a ->
                m p1 p2 (MapRList Name ctx, a)
gopenBinding f_ret mb_a =
  gcaptureCC $ \k ->
  f_ret $ flip nuMultiWithElim1 mb_a $ \names a ->
  k (names, a)

gopenBinding1 :: GenMonadCaptureCC rin rout m p1 p2 =>
                 (Binding a rin -> rout) -> Binding a b ->
                 m p1 p2 (Name a, b)
gopenBinding1 f_ret mb_b =
  gopenBinding f_ret mb_b >>>= \(_ :>: nm, b) -> greturn (nm,b)


{-
class GenMonad m => GenMonadShift r m | m -> r where
  gshift :: ((a -> m p1 p1 (r p2)) -> m p3 p4 (r p3)) -> m p2 p4 a

instance GenMonadShift r (GenContM r) where
  gshift f = GenContM $ \k -> runGenContM (f (\a -> greturn (k a))) id
-}

{-
-- | Running generalized computations in "parallel"
class GenMonad m => GenMonadPar r m p1 p2 | m p1 p2 -> r where
  gparallel :: (r -> r -> r) -> m p1 p2 a -> m p1 p2 a -> m p1 p2 a

instance GenMonadPar r2 GenContM r1 r2 where
  gparallel f (GenContM m1) (GenContM m2) =
    GenContM $ \k -> f (m1 k) (m2 k)

instance GenMonadPar r m q1 q2 =>
         GenMonadPar r (GenStateT m) '(s1,q1) '(s2,q2) where
  gparallel f m1 m2 =
    GenStateT $ \s ->
    gparallel f
    (unGenStateT m1 s) (unGenStateT m2 s)
-}

type family Fst (p :: (k1,k2)) :: k1 where
  Fst '(x,_) = x

type family Snd (p :: (k1,k2)) :: k2 where
  Snd '(_,y) = y

-- | The generalized state monad. Don't get confused: the parameters are
-- reversed, so @p2@ is the /input/ state param type and @p1@ is the /output/.
newtype GenStateT (m :: k -> k -> Kind.* -> Kind.*) p1 p2 a =
  GenStateT { unGenStateT :: Fst p2 -> m (Snd p1) (Snd p2) (a, Fst p1) }

-- | This version of 'unGenStateT' tells GHC to make the parameters into actual
-- type-level pairs, i.e., it makes GHC reason extensionally about pairs
runGenStateT :: GenStateT m '(s1,q1) '(s2,q2) a -> s2 -> m q1 q2 (a, s1)
runGenStateT = unGenStateT

instance Monad (m (Snd p) (Snd p)) => Functor (GenStateT m p p) where
  fmap f m = m >>= return . f

instance Monad (m (Snd p) (Snd p)) => Applicative (GenStateT m p p) where
  pure = return
  (<*>) = ap

instance Monad (m (Snd p) (Snd p)) => Monad (GenStateT m p p) where
  return x = GenStateT $ \s -> return (x, s)
  (GenStateT m) >>= f =
    GenStateT $ \s -> m s >>= \(a, s') -> unGenStateT (f a) s'

instance GenMonadT GenStateT q1 q2 '(s,q1) '(s,q2) where
  glift m = GenStateT $ \s -> m >>>= \a -> greturn (a, s)

instance GenMonad m => GenMonad (GenStateT m) where
  greturn x = GenStateT $ \s -> greturn (x, s)
  (GenStateT m) >>>= f =
    GenStateT $ \s -> m s >>>= \(a, s') -> unGenStateT (f a) s'

-- | FIXME: documentation
gliftGenStateT :: (GenMonad m) =>
                  m q1 q2 a -> GenStateT m '(s, q1) '(s, q2) a
gliftGenStateT m = glift m

-- | Run a generalized state computation with a different state type @s2@ inside
-- one with state type @s1@, using a lens-like pair of a getter function to
-- extract out the starting inner @s2@ state from the outer @s1@ state and an
-- update function to update the resulting outer @s1@ state with the final inner
-- @s2@ state.
withAltStateM :: GenMonad m => (s2' -> s2) -> (s2' -> s1 -> s1') ->
                 GenStateT m '(s1,q1) '(s2,q2) a ->
                 GenStateT m '(s1',q1) '(s2',q2) a
withAltStateM s_get s_update m =
  gget >>>= \s ->
  gput (s_get s) >>>
  m >>>= \a ->
  gget >>>= \s' ->
  gput (s_update s s') >>>
  greturn a

instance (Monad (m (Snd p) (Snd p)), s ~ Fst p) =>
         MonadState s (GenStateT m p p) where
  get = GenStateT $ \s -> return (s, s)
  put s = GenStateT $ \_ -> return ((), s)

class GenMonad m => GenMonadGet s m p | m p -> s where
  gget :: m p p s

class GenMonad m => GenMonadPut s m p1 p2 | m p1 p2 -> s where
  gput :: s -> m p1 p2 ()

instance GenMonad m => GenMonadGet s (GenStateT m) '(s,q) where
  gget = GenStateT $ \s -> greturn (s, s)

instance GenMonad m =>
         GenMonadPut s1 (GenStateT m) '(s1,q) '(s2,q) where
  gput s = GenStateT $ \_ -> greturn ((), s)

gmodify :: (GenMonadGet s1 m p2, GenMonadPut s2 m p1 p2) =>
           (s1 -> s2) -> m p1 p2 ()
gmodify f =
  gget >>>= \s -> gput (f s)

{-
class GenMonad m =>
      GenMonadState (s :: sk -> Type) (m :: k -> k -> Type -> Type) | m -> s where
  type GStateParam m (p :: k) :: sk
  gget :: m p p (s (GStateParam m p))
  gput :: s (GStateParam m p) -> m p p ()

instance GenMonad m => GenMonadState s (GenStateT s m) where
  type GStateParam (GenStateT s m) p = Fst p
  gget = GenStateT $ \s -> greturn (s, s)
  gput s = GenStateT $ \_ -> greturn ((), s)
-}


-- | The generalized state-continuation monad
type GenStateContM s1 r1 s2 r2 = GenStateT GenContM '(s1,r1) '(s2,r2)

{-
-- | Change both the state and return types for the state-continuation monad
withAltContStateM :: (r2 qin -> r1 qin) -> (r1 qout -> r2 qout) ->
                     (s2 pout -> s1 pout) -> (s2 pout -> s1 pin -> s2 pin) ->
                     GenStateContM s1 r1 pin qin pout qout a ->
                     GenStateContM s2 r2 pin qin pout qout a
withAltContStateM f_in f_out s_get s_update m =
  gget >>>= \s ->
  glift (withAltContM f_in f_out $
         runGenStateT m $ s_get s) >>>= \(a,s') ->
  gput (s_update s s') >>>
  greturn a
-}

-- | Map both the state and return types for the state-continuation monad
gmapRetAndState :: (s2 -> s1) -> (r1 -> r2) -> GenStateContM s1 r1 s2 r2 ()
gmapRetAndState f_st f_ret =
  gmodify f_st >>> gmapRet f_ret

-- | Run two generalized monad computations "in parallel" and combine their
-- results
--
-- FIXME: figure out an elegant way to write this as a gmonad effect
gparallel :: (r1 -> r2 -> r_out) ->
             GenStateContM s_in r_in s_out r1 a ->
             GenStateContM s_in r_in s_out r2 a ->
             GenStateContM s_in r_in s_out r_out a
gparallel f m1 m2 =
  GenStateT $ \s -> GenContM $ \k ->
  f (runGenContM (unGenStateT m1 s) k) (runGenContM (unGenStateT m2 s) k)

-- | Abort the current state-continuation computation and just return an @r2@
--
-- FIXME: figure out how to write this with something like 'gcaptureCC'...? The
-- problem is that 'gcaptureCC' will not allow us to change the state type...
gabortM :: r2 -> GenStateContM s1 r1 s2 r2 a
gabortM ret = GenStateT $ \_ -> GenContM $ \_ -> ret


----------------------------------------------------------------------
-- * Permission Implication Monad
----------------------------------------------------------------------

data ImplState vars ps =
  ImplState { _implStatePerms :: PermSet ps,
              _implStateVars :: CruCtx vars,
              _implStatePSubst :: PartialSubst vars,
              _implStateLifetimes :: [ExprVar LifetimeType],
              -- ^ Stack of active lifetimes, where the first element is the
              -- current lifetime (we should have an @lowned@ permission for it)
              -- and each lifetime contains (i.e., has an @lcurrent@ permission
              -- for) the next lifetime in the stack
              _implStateMuRecurseFlag :: Maybe (Either () ()),
              -- ^ Whether we are recursing under a @mu@, either on the left
              -- hand or the right hand side
              _implStateFnEnv :: FunTypeEnv
              -- ^ The current function environment
            }
makeLenses ''ImplState

mkImplState :: CruCtx vars -> PermSet ps -> FunTypeEnv -> ImplState vars ps
mkImplState vars perms env =
  ImplState { _implStateVars = vars,
              _implStatePerms = perms,
              _implStatePSubst = emptyPSubst vars,
              _implStateLifetimes = [],
              _implStateMuRecurseFlag = Nothing,
              _implStateFnEnv = env
            }

extImplState :: KnownRepr TypeRepr tp => ImplState vars ps ->
                ImplState (vars :> tp) ps
extImplState s =
  s { _implStateVars = extCruCtx (_implStateVars s),
      _implStatePSubst = extPSubst (_implStatePSubst s) }

unextImplState :: ImplState (vars :> a) ps -> ImplState vars ps
unextImplState s =
  s { _implStateVars = unextCruCtx (_implStateVars s),
      _implStatePSubst = unextPSubst (_implStatePSubst s) }

-- | The implication monad is the permission monad that uses 'ImplState'
type ImplM vars r ps_out ps_in =
  GenStateContM (ImplState vars ps_out) (PermImpl r ps_out)
  (ImplState vars ps_in) (PermImpl r ps_in)


-- | Run an 'ImplM' computation by passing it a @vars@ context, a starting
-- permission set, and an @r@
runImplM :: CruCtx vars -> PermSet ps_in -> FunTypeEnv -> r ps_out ->
            ImplM vars r ps_out ps_in () -> PermImpl r ps_in
runImplM vars perms env r m =
  runGenContM (runGenStateT m (mkImplState vars perms env))
  (const $ PermImpl_Done r)

-- | Look up the current partial substitution
getPSubst :: ImplM vars r ps ps (PartialSubst vars)
getPSubst = view implStatePSubst <$> gget

-- | Apply the current partial substitution to an expression, raising an error
-- with the given string if the partial substitution is not complete enough
partialSubstForceM :: Substable PartialSubst a Maybe => Mb vars a -> String ->
                      ImplM vars r ps ps a
partialSubstForceM mb_e msg =
  getPSubst >>>= \psubst ->
  greturn (partialSubstForce psubst mb_e msg)

-- | Modify the current partial substitution
modifyPSubst :: (PartialSubst vars -> PartialSubst vars) ->
                ImplM vars r ps ps ()
modifyPSubst f = gmodify (over implStatePSubst f)

-- | Set the value for an existential variable in the current substitution,
-- raising an error if it is already set
setVarM :: Member vars a -> PermExpr a -> ImplM vars r ps ps ()
setVarM x e = modifyPSubst (psubstSet x e)

-- | Run an implication computation with one more existential variable,
-- returning the optional expression it was bound to in the current partial
-- substitution when it is done
withExtVarsM :: KnownRepr TypeRepr tp =>
                ImplM (vars :> tp) r ps1 ps2 a ->
                ImplM vars r ps1 ps2 (a, Maybe (PermExpr tp))
withExtVarsM m =
  withAltStateM extImplState (const unextImplState) $
  (m >>>= \a ->
    getPSubst >>>= \psubst ->
    greturn (a, psubstLookup psubst Member_Base))

-- | Set the mu recursion flag to indicate recursion on the right, or fail if we
-- are already recursing on the left
implSetMuRecurseRightM :: ImplM vars r ps ps ()
implSetMuRecurseRightM =
  gget >>>= \s ->
  case view implStateMuRecurseFlag s of
    Just (Left ()) -> implFailM
    _ -> gmodify (set implStateMuRecurseFlag (Just (Right ())))

-- | Set the mu recursion flag to indicate recursion on the left, or fail if we
-- are already recursing on the right
implSetMuRecurseLeftM :: ImplM vars r ps ps ()
implSetMuRecurseLeftM =
  gget >>>= \s ->
  case view implStateMuRecurseFlag s of
    Just (Right ()) -> implFailM
    _ -> gmodify (set implStateMuRecurseFlag (Just (Left ())))

-- | Get the current 'PermSet'
getPerms :: ImplM vars r ps ps (PermSet ps)
getPerms = view implStatePerms <$> gget

-- | Look up the current permission for a given variable
getPerm :: ExprVar a -> ImplM vars r ps ps (ValuePerm a)
getPerm x = view (implStatePerms . varPerm x) <$> gget

-- | Get the pointer permissions for a variable @x@, assuming @x@ has LLVM
-- pointer permissions
getLLVMPtrPerms :: ExprVar (LLVMPointerType w) ->
                   ImplM vars r ps ps [LLVMPtrPerm w]
getLLVMPtrPerms x =
  view (implStatePerms . varPerm x . llvmPtrPerms) <$> gget

-- | Get the distinguished permission stack
getDistPerms :: ImplM vars r ps ps (DistPerms ps)
getDistPerms = view (implStatePerms . distPerms) <$> gget

-- | Get the top permission in the stack
getTopDistPerm :: ExprVar a -> ImplM vars r (ps :> a) (ps :> a) (ValuePerm a)
getTopDistPerm x = view (implStatePerms . topDistPerm x) <$> gget

-- | Set the current 'PermSet'
setPerms :: PermSet ps -> ImplM vars r ps ps ()
setPerms perms = modify $ set (implStatePerms) perms

-- | Set the current permission for a given variable
setPerm :: ExprVar a -> ValuePerm a -> ImplM vars r ps ps ()
setPerm x p = modify $ set (implStatePerms . varPerm x) p

-- | Get the current lifetime
getCurLifetime :: ImplM vars r ps ps (ExprVar LifetimeType)
getCurLifetime =
  (view implStateLifetimes <$> gget) >>>= \ls ->
  case ls of
    l:_ -> greturn l
    [] -> error "getCurLifetime: no current lifetime!"

-- | Push a lifetime onto the lifetimes stack
pushLifetime :: ExprVar LifetimeType -> ImplM vars r ps ps ()
pushLifetime l = gmodify (over implStateLifetimes (l:))

-- | Pop a lifetime off of the lifetimes stack
popLifetime :: ImplM vars r ps ps (ExprVar LifetimeType)
popLifetime =
  getCurLifetime >>>= \l ->
  gmodify (over implStateLifetimes tail) >>>
  greturn l

-- | Push (as in 'implPushM') the permission for the current lifetime
implPushCurLifetimePermM :: ExprVar LifetimeType ->
                            ImplM vars r (ps :> LifetimeType) ps ()
implPushCurLifetimePermM l =
  getCurLifetime >>>= \l' ->
  (if l == l' then greturn () else
     error "implPushLifetimePermM: wrong value for the current lifetime!") >>>
  getPerm l >>>= \p ->
  case p of
    ValPerm_Conj [Perm_LOwned _] -> implPushM l p
    _ -> error "implPushLifetimePermM: no LOwned permission for the current lifetime!"

-- | Pop (as in 'implPushM') the permission for the current lifetime
implPopCurLifetimePermM :: ExprVar LifetimeType ->
                           ImplM vars r ps (ps :> LifetimeType) ()
implPopCurLifetimePermM l =
  getCurLifetime >>>= \l' ->
  (if l == l' then greturn () else
     error "implPopLifetimePermM: wrong value for the current lifetime!") >>>
  getTopDistPerm l >>>= \p ->
  case p of
    ValPerm_Conj [Perm_LOwned _] -> implPopM l p
    _ -> error "implPopLifetimePermM: no LOwned permission for the current lifetime!"

{- FIXME: this should no longer be needed!
-- | Map the final return value and the current permissions
gmapRetAndPerms :: (PermSet ps2 -> PermSet ps1) ->
                   (PermImpl r ps1 -> PermImpl r ps2) ->
                   ImplM vars r ps1 ps2 ()
gmapRetAndPerms f_perms f_impl =
  gmapRetAndState (over implStatePerms f_perms) f_impl
-}


----------------------------------------------------------------------
-- * The Permission Implication Rules as Monadic Operations
----------------------------------------------------------------------

-- | An 'ImplM' continuation for a permission implication rule
newtype Impl1Cont vars r ps_r a bs_ps =
  Impl1Cont (MapRList Name (Fst bs_ps) -> ImplM vars r ps_r (Snd bs_ps) a)

-- | Apply a permission implication rule, with the given continuations in the
-- possible disjunctive branches of the result
implApplyImpl1 :: PermImpl1 ps_in ps_outs ->
                  MapRList (Impl1Cont vars r ps_r a) ps_outs ->
                  ImplM vars r ps_r ps_in a
implApplyImpl1 impl1 mb_ms =
  getPerms >>>= \perms ->
  gmapRet (PermImpl_Step impl1) >>>
  helper (applyImpl1 impl1 perms) mb_ms
  where
    helper :: MbPermSets ps_outs ->
              MapRList (Impl1Cont vars r ps_r a) ps_outs ->
              GenStateContM (ImplState vars ps_r) (PermImpl r ps_r)
              (ImplState vars ps_in) (MbPermImpls r ps_outs) a
    helper MbPermSets_Nil _ = gabortM MbPermImpls_Nil
    helper (MbPermSets_Cons mbperms mbperm) (args :>: Impl1Cont f) =
      gparallel MbPermImpls_Cons (helper mbperms args)
      (gopenBinding id mbperm >>>= \(ns, perms') ->
        gmodify (set implStatePerms perms') >>>
        f ns)

-- | Terminate the current proof branch with a failure
implFailM :: ImplM vars r ps_any ps a
implFailM = implApplyImpl1 Impl1_Fail MNil

-- | Produce a branching proof tree that performs the first implication and, if
-- that one fails, falls back on the second
implCatchM :: ImplM vars r ps1 ps2 () -> ImplM vars r ps1 ps2 () ->
              ImplM vars r ps1 ps2 ()
implCatchM m1 m2 =
  implApplyImpl1 Impl1_Catch
  (MNil :>: Impl1Cont (const m1) :>: Impl1Cont (const m2))

-- | "Push" all of the permissions in the permission set for a variable, which
-- should be equal to the supplied permission, after deleting those permissions
-- from the input permission set. This is like a simple "proof" of @x:p@.
implPushM :: ExprVar a -> ValuePerm a -> ImplM vars r (ps :> a) ps ()
implPushM x p =
  implApplyImpl1 (Impl1_Push x p) (MNil :>: Impl1Cont (const $ greturn ()))

-- | Pop a permission from the top of the stack back to the primary permission
-- for a variable, assuming that the primary permission for that variable is
-- empty, i.e., is the @true@ permission
implPopM :: ExprVar a -> ValuePerm a -> ImplM vars r ps (ps :> a) ()
implPopM x p =
  implApplyImpl1 (Impl1_Pop x p) (MNil :>: Impl1Cont (const $ greturn ()))

-- | Eliminate a disjunctive permission @x:(p1 \/ p2)@, building proof trees
-- that proceed with both @x:p1@ and @x:p2@
implElimOrM :: ExprVar a -> ValuePerm a -> ValuePerm a ->
               ImplM vars r (ps :> a) (ps :> a) ()
implElimOrM x p1 p2 =
  implApplyImpl1 (Impl1_ElimOr x p1 p2)
  (MNil :>: Impl1Cont (const $ greturn ()) :>: Impl1Cont (const $ greturn ()))

-- | Eliminate an existential permission @x:(exists (y:tp).p)@ in the current
-- permission set
implElimExistsM :: KnownRepr TypeRepr tp =>
                   ExprVar a -> Binding tp (ValuePerm a) ->
                   ImplM vars r (ps :> a) (ps :> a) ()
implElimExistsM x p =
  implApplyImpl1 (Impl1_ElimExists x p)
  (MNil :>: Impl1Cont (const $ greturn ()))

-- | Apply a simple implication rule to the top permissions on the stack
implSimplM :: Proxy ps -> SimplImpl ps_in ps_out ->
              ImplM vars r (ps :++: ps_out) (ps :++: ps_in) ()
implSimplM prx simpl =
  implApplyImpl1 (Impl1_Simpl simpl prx)
  (MNil :>: Impl1Cont (const $ greturn ()))

-- | Eliminate a permission @x:ptr((rw,off) |-> p)@ into permissions
-- @x:ptr((rw,off) |-> eq(y))@ and @y:p@ for a fresh variable @y@, returning the
-- fresh variable @y@ and popping the @y@ permissions off the stack. If @p@
-- already has the form @eq(y)@, then just return @y@.
implElimLLVMFieldContentsM ::
  (1 <= w, KnownNat w) => ExprVar (LLVMPointerType w) -> LLVMFieldPerm w ->
  ImplM vars r (ps :> LLVMPointerType w) (ps :> LLVMPointerType w)
  (ExprVar (LLVMPointerType w))
implElimLLVMFieldContentsM _ fp
  | ValPerm_Eq (PExpr_Var y) <- llvmFieldContents fp
  = greturn y
implElimLLVMFieldContentsM x fp =
  implApplyImpl1 (Impl1_ElimLLVMFieldContents x fp)
  (MNil :>: Impl1Cont (\(_ :>: n) -> greturn n)) >>>= \y ->
  implPopM y (llvmFieldContents fp) >>>
  greturn y

-- | Try to prove a proposition about bitvectors dynamically, failing as in
-- 'implFailM' if the proposition does not hold
implTryProveBVProp :: (1 <= w, KnownNat w) =>
                      ExprVar (LLVMPointerType w) -> BVProp w ->
                      ImplM vars r (ps :> LLVMPointerType w) ps ()
implTryProveBVProp x p =
  implApplyImpl1 (Impl1_TryProveBVProp x p)
  (MNil :>: Impl1Cont (const $ greturn ()))

-- | Drop a permission from the top of the stack
implDropM :: ExprVar a -> ValuePerm a -> ImplM vars r ps (ps :> a) ()
implDropM x p = implSimplM Proxy (SImpl_Drop x p)

-- | Swap the top two permissions on the top of the stack
implSwapM :: ExprVar a -> ValuePerm a -> ExprVar b -> ValuePerm b ->
             ImplM vars r (ps :> b :> a) (ps :> a :> b) ()
implSwapM x p1 y p2 = implSimplM Proxy (SImpl_Swap x p1 y p2)

-- | Eliminate disjunctives and existentials on the top of the stack and return
-- the resulting permission
elimOrsExistsM :: ExprVar a -> ImplM vars r (ps :> a) (ps :> a) (ValuePerm a)
elimOrsExistsM x =
  getTopDistPerm x >>>= \p ->
  case p of
    ValPerm_Or p1 p2 -> implElimOrM x p1 p2 >>> elimOrsExistsM x
    ValPerm_Exists mb_p ->
      implElimExistsM x mb_p >>> elimOrsExistsM x
    _ -> greturn p

-- | Introduce a proof of @x:eq(x)@ onto the top of the stack
introEqReflM :: ExprVar a -> ImplM vars r (ps :> a) ps ()
introEqReflM x = implSimplM Proxy (SImpl_IntroEqRefl x)

-- | Copy an @x:eq(e)@ permission on the top of the stack
introEqCopyM :: ExprVar a -> PermExpr a ->
                ImplM vars r (ps :> a :> a) (ps :> a) ()
introEqCopyM x e = implSimplM Proxy (SImpl_CopyEq x e)

-- | Cast a @y:p@ perm on the top of the stack to an @x:p@ perm using an
-- @x:eq(y)@ just below it on the stack
introCastM :: ExprVar a -> ExprVar a -> ValuePerm a ->
              ImplM vars r (ps :> a) (ps :> a :> a) ()
introCastM x y p = implSimplM Proxy (SImpl_Cast x y p)

-- | Introduce a proof of @x:true@ onto the top of the stack, which is the same
-- as an empty conjunction
introConjM :: ExprVar a -> ImplM vars r (ps :> a) ps ()
introConjM x = implSimplM Proxy (SImpl_IntroConj x)

-- | Delete the @i@th atomic permission in the conjunct on the top of the stack,
-- assuming that conjunction contains the given atomic permissions, and put the
-- extracted atomic permission just below the top of the stack
implExtractConjM :: ExprVar a -> [AtomicPerm a] -> Int ->
                    ImplM vars r (ps :> a :> a) (ps :> a) ()
implExtractConjM x ps i = implSimplM Proxy (SImpl_ExtractConj x ps i)

-- | Copy @i@th atomic permission in the conjunct on the top of the stack,
-- assuming that conjunction contains the given atomic permissions and that the
-- given conjunct is copyable, and put the copied atomic permission just below
-- the top of the stack
implCopyConjM :: ExprVar a -> [AtomicPerm a] -> Int ->
                 ImplM vars r (ps :> a :> a) (ps :> a) ()
implCopyConjM x ps i = implSimplM Proxy (SImpl_CopyConj x ps i)

-- | Insert an atomic permission below the top of the stack at the @i@th
-- position in the conjunct on the top of the stack, where @i@ must be between
implInsertConjM :: ExprVar a -> AtomicPerm a -> [AtomicPerm a] -> Int ->
                   ImplM vars r (ps :> a) (ps :> a :> a) ()
implInsertConjM x p ps i = implSimplM Proxy (SImpl_InsertConj x p ps i)

-- | Apply the left or-introduction rule to the top of the permission stack,
-- changing it from @x:p1@ to @x:(p1 \/ p2)@
introOrLM :: ExprVar a -> ValuePerm a -> ValuePerm a ->
             ImplM vars r (ps :> a) (ps :> a) ()
introOrLM x p1 p2 = implSimplM Proxy (SImpl_IntroOrL x p1 p2)

-- | Apply the right or-introduction rule to the top of the permission stack,
-- changing it from @x:p2@ to @x:(p1 \/ p2)@
introOrRM :: ExprVar a -> ValuePerm a -> ValuePerm a ->
             ImplM vars r (ps :> a) (ps :> a) ()
introOrRM x p1 p2 = implSimplM Proxy (SImpl_IntroOrR x p1 p2)

-- | Apply existential introduction to the top of the permission stack, changing
-- it from @[e/x]p@ to @exists (x:tp).p@
--
-- FIXME: is there some way we could "type-check" this, to ensure that the
-- permission on the top of the stack really equals @[e/x]p@?
introExistsM :: KnownRepr TypeRepr tp => ExprVar a -> PermExpr tp ->
                Binding tp (ValuePerm a) ->
                ImplM vars r (ps :> a) (ps :> a) ()
introExistsM x e p_body = implSimplM Proxy (SImpl_IntroExists x e p_body)

-- | Cast a proof of @x:eq(LLVMWord(e1))@ to @x:eq(LLVMWord(e2))@ on the top of
-- the stack
castLLVMWordEqM ::
  (1 <= w, KnownNat w) =>
  ExprVar (LLVMPointerType w) -> PermExpr (BVType w) -> PermExpr (BVType w) ->
  ImplM vars r (ps :> LLVMPointerType w) (ps :> LLVMPointerType w) ()
castLLVMWordEqM x e1 e2 =
  implTryProveBVProp x (BVProp_Eq e1 e2) >>>
  implSimplM Proxy (SImpl_CastLLVMWord x e1 e2)

-- | Cast a @y:ptr(pps)@ on the top of the stack to @x:ptr(pps - off)@ using a
-- proof of @x:eq(y+off)@ just below it on the stack
castLLVMPtrM :: (1 <= w, KnownNat w) => ExprVar (LLVMPointerType w) ->
                [AtomicPerm (LLVMPointerType w)] -> PermExpr (BVType w) ->
                ExprVar (LLVMPointerType w) ->
                ImplM vars r (ps :> LLVMPointerType w)
                (ps :> LLVMPointerType w :> LLVMPointerType w) ()
castLLVMPtrM y ps off x = implSimplM Proxy (SImpl_CastLLVMPtr y ps off x)

-- | Cast a proof of @x:free(e1)@ on the top of the stack to one of @x:free(e2)@
-- by first proving that @e1=e2@
castLLVMFreeM :: (1 <= w, KnownNat w) => ExprVar (LLVMPointerType w) ->
                 PermExpr (BVType w) -> PermExpr (BVType w) ->
                 ImplM vars r (ps :> LLVMPointerType w)
                 (ps :> LLVMPointerType w) ()
castLLVMFreeM x e1 e2 =
  implTryProveBVProp x (BVProp_Eq e1 e2) >>>
  implSimplM Proxy (SImpl_CastLLVMFree x e1 e2)

-- | Build a proof of @mu X.p@ from one of @[mu X.p/X]p@
implFoldMuM :: ExprVar a -> Binding (ValuePerm a) (ValuePerm a) ->
               ImplM vars r (ps :> a) (ps :> a) ()
implFoldMuM x p_body = implSimplM Proxy (SImpl_FoldMu x p_body)

-- | Build a proof of @[mu X.p/X]p@ from one of @mu X.p@
implUnfoldMuM :: ExprVar a -> Binding (ValuePerm a) (ValuePerm a) ->
                 ImplM vars r (ps :> a) (ps :> a) ()
implUnfoldMuM x p_body = implSimplM Proxy (SImpl_UnfoldMu x p_body)

-- | FIXME: document this!
implSplitLifetimeM :: KnownRepr TypeRepr a => ExprVar a -> ValuePerm a ->
                      ExprVar LifetimeType ->
                      ImplM vars r (ps :> a :> LifetimeType)
                      (ps :> a :> LifetimeType) ()
implSplitLifetimeM x p l =
  getTopDistPerm l >>>= \lp ->
  case lp of
    ValPerm_Conj [Perm_LOwned ps] ->
      implSimplM Proxy (SImpl_SplitLifetime x p l ps)
    _ -> error "implSplitLifetimeM: top permission is not an lowned permission"

-- | Combine proofs of @x:ptr(pps,(off,spl) |-> eq(y))@ and @y:p@ on the top of
-- the permission stack into a proof of @x:ptr(pps,(off,spl |-> p))@
introLLVMFieldContentsM ::
  ExprVar (LLVMPointerType w) ->
  ExprVar (LLVMPointerType w) ->
  ImplM vars r (ps :> LLVMPointerType w)
  (ps :> LLVMPointerType w :> LLVMPointerType w) ()
introLLVMFieldContentsM x y =
  getDistPerms >>>= \ps ->
  case ps of
    DistPermsCons
      (DistPermsCons _ x'
       (ValPerm_Conj [Perm_LLVMField fp@(LLVMFieldPerm
                                         { llvmFieldContents =
                                             ValPerm_Eq (PExpr_Var y')})]))
      y'' p
      | x' == x && y' == y && y'' == y ->
        implSimplM Proxy (SImpl_IntroLLVMFieldContents x y $
                          fp { llvmFieldContents = p })
    _ -> error "introLLVMFieldContentsM: input perms not in expected form"


-- | Call 'implPushM' for multiple @x:p@ permissions
implPushMultiM :: DistPerms ps -> ImplM vars r ps RNil ()
implPushMultiM DistPermsNil = greturn ()
implPushMultiM (DistPermsCons ps x p) =
  implPushMultiM ps >>> implPushM x p


----------------------------------------------------------------------
-- * Recombining Permissions
----------------------------------------------------------------------

-- | Recombine the permission @x:p@ on top of the stack back into the existing
-- permission @x_p@ for @x@, where @x_p@ is the first permission argument and
-- @p@ is the second
recombinePerm :: ExprVar a -> ValuePerm a -> ValuePerm a ->
                 ImplM RNil r as (as :> a) ()
recombinePerm x _ p@ValPerm_True = implDropM x p
recombinePerm x ValPerm_True p = implPopM x p
recombinePerm x x_p@(ValPerm_Eq (PExpr_Var y)) p =
  implPushM x x_p >>> introEqCopyM x (PExpr_Var y) >>> implPopM x x_p >>>
  implSwapM x p x x_p >>> introCastM x y p >>> getPerm y >>>= \y_p ->
  recombinePerm y y_p p
recombinePerm x x_p@(ValPerm_Eq (PExpr_LLVMOffset y off)) (ValPerm_Conj ps) =
  implPushM x x_p >>> introEqCopyM x (PExpr_LLVMOffset y off) >>>
  implPopM x x_p >>> implSimplM Proxy (SImpl_InvertLLVMOffsetEq x off y) >>>
  castLLVMPtrM x ps (bvNegate off) y >>>
  getPerm y >>>= \y_p ->
  recombinePerm y y_p (ValPerm_Conj $ map (offsetLLVMAtomicPerm off) ps)
recombinePerm _ _ (ValPerm_Eq _) =
  -- NOTE: we could handle this by swapping the stack with the variable perm and
  -- calling recombinePerm again, but this could potentially create permission
  -- equality cycles with, e.g., x:eq(y) * y:eq(x). Plus, we don't expect any
  -- functions or typed instructions to return equality permissions unless it is
  -- for a new, fresh variable, in which case the above cases will handle it
  error "recombinePerm: unexpected equality permission being recombined"
recombinePerm x x_p (ValPerm_Or _ _) =
  elimOrsExistsM x >>>= \p' -> recombinePerm x x_p p'
recombinePerm x x_p (ValPerm_Exists _) =
  elimOrsExistsM x >>>= \p' -> recombinePerm x x_p p'
recombinePerm x x_p@(ValPerm_Or _ _) p =
  implPushM x x_p >>> elimOrsExistsM x >>>= \x_p' ->
  implPopM x x_p' >>> recombinePerm x x_p' p
recombinePerm x x_p@(ValPerm_Exists _) p =
  implPushM x x_p >>> elimOrsExistsM x >>>= \x_p' ->
  implPopM x x_p' >>> recombinePerm x x_p' p
recombinePerm x x_p@(ValPerm_Conj x_ps) (ValPerm_Conj (p:ps)) =
  implExtractConjM x ps 0 >>>
  implSwapM x (ValPerm_Conj1 p) x (ValPerm_Conj ps) >>>
  implPushM x x_p >>> implInsertConjM x p x_ps (length x_ps) >>>
  implPopM x (ValPerm_Conj (x_ps ++ [p])) >>>
  recombinePerm x (ValPerm_Conj (x_ps ++ [p])) (ValPerm_Conj ps)
recombinePerm x _ p = implDropM x p

-- | Recombine the permissions on the stack back into the permission set
recombinePerms :: DistPerms ps -> ImplM RNil r RNil ps ()
recombinePerms DistPermsNil = greturn ()
recombinePerms (DistPermsCons ps' x p) =
  getPerm x >>>= \x_p -> recombinePerm x x_p p >>> recombinePerms ps'


----------------------------------------------------------------------
-- * Proving Equality Permissions
----------------------------------------------------------------------

-- | Build a proof on the top of the stack that @x:eq(e)@. Assume that all @x@
-- permissions are on the top of the stack and given by argument @p@, and pop
-- them back to the primary permission for @x@ at the end of the proof.
proveVarEq :: NuMatchingAny1 r => ExprVar a -> ValuePerm a ->
              Mb vars (PermExpr a) -> ImplM vars r (ps :> a) (ps :> a) ()
proveVarEq x p mb_e =
  getPSubst >>>= \psubst ->
  proveVarEqH x p psubst mb_e

-- | Main helper function for 'proveVarEq'
proveVarEqH :: NuMatchingAny1 r => ExprVar a -> ValuePerm a ->
               PartialSubst vars -> Mb vars (PermExpr a) ->
               ImplM vars r (ps :> a) (ps :> a) ()

-- Prove x:eq(z) for evar z by setting z=x
proveVarEqH x p psubst [nuP| PExpr_Var z |]
  | Left memb <- mbNameBoundP z
  , Nothing <- psubstLookup psubst memb
  = setVarM memb (PExpr_Var x) >>> implPopM x p >>> introEqReflM x

-- Prove x:eq(x) by reflexivity
proveVarEqH x p psubst mb_e
  | Just (PExpr_Var y) <- partialSubst psubst mb_e
  , x == y
  = implPopM x p >>> introEqReflM x

-- Prove x:eq(e) |- x:eq(e) using introEqCopyM
proveVarEqH x p@(ValPerm_Eq e) psubst mb_e
  | Just e' <- partialSubst psubst mb_e
  , e' == e
  = introEqCopyM x e >>> implPopM x p

-- Prove x:eq(word(e)) |- x:eq(word(z)) by setting z=e
proveVarEqH x p@(ValPerm_Eq
                 (PExpr_LLVMWord e)) psubst [nuP| PExpr_LLVMWord (PExpr_Var z) |]
  | Left memb <- mbNameBoundP z
  , Nothing <- psubstLookup psubst memb
  = setVarM memb e >>> introEqCopyM x (PExpr_LLVMWord e) >>> implPopM x p

-- Prove x:eq(word(e')) |- x:eq(word(e)) by first proving x:eq(word(e')) and
-- then casting e' to e
proveVarEqH x p@(ValPerm_Eq
                 (PExpr_LLVMWord e')) psubst [nuP| PExpr_LLVMWord mb_e |]
  | Just e <- partialSubst psubst mb_e =
    introEqCopyM x (PExpr_LLVMWord e') >>> implPopM x p >>>
    castLLVMWordEqM x e' e

-- Eliminate disjunctive and/or existential permissions
proveVarEqH x (ValPerm_Or _ _) _ mb_e =
  elimOrsExistsM x >>>= \p -> proveVarEq x p mb_e

-- Eliminate disjunctive and/or existential permissions
proveVarEqH x (ValPerm_Exists _) _ mb_e =
  elimOrsExistsM x >>>= \p -> proveVarEq x p mb_e

-- Otherwise give up!
proveVarEqH _ _ _ _ = implFailM


----------------------------------------------------------------------
-- * Proving Field Permissions
----------------------------------------------------------------------

-- | Prove an LLVM field permission @x:ptr((rw,off) |-> p)@ from permission @pi@
-- assuming that the the current permissions @x:(p1 * ... *pn)@ for @x@ are on
-- the top of the stack, and ensuring that any remaining permissions for @x@ get
-- popped back to the primary permissions for @x@
proveVarLLVMField ::
  (1 <= w, KnownNat w, NuMatchingAny1 r) => ExprVar (LLVMPointerType w) ->
  [AtomicPerm (LLVMPointerType w)] -> Int ->
  PermExpr (BVType w) -> Mb vars (LLVMFieldPerm w) ->
  ImplM vars r (ps :> LLVMPointerType w) (ps :> LLVMPointerType w) ()
proveVarLLVMField x ps i off mb_fp =
  implExtractConjM x ps i >>>
  let ps_rem = take i ps ++ drop (i+1) ps in
  implPopM x (ValPerm_Conj ps_rem) >>>
  extractNeededLLVMFieldPerm x (ps!!i) off mb_fp >>>= \(fp,maybe_p_rem) ->
  (case maybe_p_rem of
      Just p_rem ->
        implPushM x (ValPerm_Conj ps_rem) >>>
        implInsertConjM x p_rem ps_rem i >>>
        implPopM x (ValPerm_Conj (take i ps_rem ++ [p_rem] ++ drop i ps_rem))
      Nothing -> implDropM x ValPerm_True) >>>
  implElimLLVMFieldContentsM x fp >>>= \y ->
  let fp' = fp { llvmFieldContents = ValPerm_Eq (PExpr_Var y) } in
  proveVarLLVMFieldH x fp' off mb_fp


-- | Cast the offset, change the lifetime if needed, and prove the contents
proveVarLLVMFieldH ::
  (1 <= w, KnownNat w, NuMatchingAny1 r) =>
  ExprVar (LLVMPointerType w) -> LLVMFieldPerm w ->
  PermExpr (BVType w) -> Mb vars (LLVMFieldPerm w) ->
  ImplM vars r (ps :> LLVMPointerType w) (ps :> LLVMPointerType w) ()
proveVarLLVMFieldH x fp off' mb_fp =
  -- Step 1: cast the field offset to off'
  implTryProveBVProp x (BVProp_Eq (llvmFieldOffset fp) off') >>>
  implSimplM Proxy (SImpl_CastLLVMFieldOffset x fp off') >>>
  -- Step 2: change the lifetime if needed
  --
  -- NOTE: we could allow existential lifetimes on the RHS by just adding some
  -- more cases here
  partialSubstForceM (fmap llvmFieldLifetime mb_fp)
  "proveVarLLVMFieldH: incomplete RHS lifetime" >>>= \l2 ->
  (case (llvmFieldLifetime fp, l2) of
      (l1, l2) | l1 == l2 -> greturn ()
      (PExpr_Always, l2) ->
        implSimplM Proxy (SImpl_LLVMFieldLifetimeAlways x fp l2)
      (PExpr_Var l1, l2) ->
        let lcur_perm = ValPerm_Conj [Perm_LCurrent l2] in
        proveVarImpl l1 (fmap (const $ lcur_perm) mb_fp) >>>
        implSimplM Proxy (SImpl_LLVMFieldLifetimeCurrent x fp l1 l2)) >>>
  
  -- Step 3: prove contents
  let y = case llvmFieldContents fp of
        ValPerm_Eq (PExpr_Var y) -> y
        _ -> error "proveVarLLVMFieldH: expected eq(y) perm contents" in
  proveVarImpl y (fmap llvmFieldContents mb_fp) >>>
  introLLVMFieldContentsM x y


-- | Extract an LLVM field permission from the given atomic permission, leaving
-- as much of the original atomic permission as possible on the top of the stack
-- (which could be none of it, i.e., @true@). At the end of this function, the
-- top of the stack should look like
--
-- > x:ptr((rw,off) -> p) * x:rem
--
-- where @rem@ is the remainder of the input atomic permission, which is either
-- a single atomic permission or @true@. The field permission and remaining
-- atomic permission (if any) are the return values of this function.
extractNeededLLVMFieldPerm ::
  NuMatchingAny1 r => ExprVar (LLVMPointerType w) ->
  AtomicPerm (LLVMPointerType w) ->
  PermExpr (BVType w) -> Mb vars (LLVMFieldPerm w) ->
  ImplM vars r (ps :> LLVMPointerType w :> LLVMPointerType w)
  (ps :> LLVMPointerType w)
  (LLVMFieldPerm w, Maybe (AtomicPerm (LLVMPointerType w)))

-- If proving x:ptr((R,off) |-> p) |- x:ptr((R,off') |-> p'), just copy the read
-- permission
extractNeededLLVMFieldPerm x (Perm_LLVMField fp) off' mb_fp
  | Read <- llvmFieldRW fp
  , Read <- mbLift (fmap llvmFieldRW mb_fp)
  = implCopyConjM x [Perm_LLVMField fp] 0 >>>
    greturn (fp, Just (Perm_LLVMField fp))

-- Cannot prove x:ptr((R,off) |-> p) |- x:ptr((W,off') |-> p'), so fail
extractNeededLLVMFieldPerm x (Perm_LLVMField fp) _ mb_fp
  | Read <- llvmFieldRW fp
  , Write <- mbLift (fmap llvmFieldRW mb_fp)
  = implFailM

-- If proving x:[l1]ptr((W,off) |-> p) |- x:[l2]ptr((R,off') |-> p'), first
-- split the lifetime of the input permission if l2 /= l1 and we have an lowned
-- permission for l2, and then demote to read and copy the read permission as in
-- the R |- R case above
extractNeededLLVMFieldPerm x p@(Perm_LLVMField fp) off' mb_fp
  | Write <- llvmFieldRW fp
  , Read <- mbLift (fmap llvmFieldRW mb_fp)
  = partialSubstForceM (fmap llvmFieldLifetime mb_fp)
    "extractNeededLLVMFieldPerm: incomplete RHS lifetime" >>>= \l2 ->
    -- NOTE: we could allow existential lifetimes on the RHS by just adding some
    -- more cases here
    (case l2 of
        PExpr_Var l2_var ->
          getPerm l2_var >>>= \l2_perm ->
          (case l2_perm of
              ValPerm_Conj [Perm_LOwned _] ->
                implPushM l2_var l2_perm >>>
                implSplitLifetimeM x (ValPerm_Conj1 p) l2_var >>>
                implPopM l2_var l2_perm >>>
                greturn (fp { llvmFieldLifetime = l2 })
              _ -> greturn fp)
        _ -> greturn fp) >>>= \fp' ->
    implSimplM Proxy (SImpl_DemoteLLVMFieldWrite x fp') >>>
    let fp'' = fp' { llvmFieldRW = Read } in
    implCopyConjM x [Perm_LLVMField fp''] 0 >>>
    greturn (fp'', Just (Perm_LLVMField fp''))

-- If proving x:ptr((W,off) |-> p) |- x:ptr((W,off') |-> p'), just push a true
-- permission, because there is no remaining permission
extractNeededLLVMFieldPerm x (Perm_LLVMField fp) off' mb_fp
  | Write <- llvmFieldRW fp
  , Write <- mbLift (fmap llvmFieldRW mb_fp)
  = introConjM x >>> greturn (fp, Nothing)

extractNeededLLVMFieldPerm x (Perm_LLVMArray ap) off' mb_fp =
  error "FIXME HERE NOW"

-- Cannot get a field permission from a free, so fail
extractNeededLLVMFieldPerm _ (Perm_LLVMFree _) _ _ = implFailM


----------------------------------------------------------------------
-- * Proving LLVM Array Permissions
----------------------------------------------------------------------

proveVarLLVMArray ::
  NuMatchingAny1 r => ExprVar (LLVMPointerType w) ->
  [AtomicPerm (LLVMPointerType w)] -> Int ->
  PermExpr (BVType w) -> Mb vars (LLVMArrayPerm w) ->
  ImplM vars r (ps :> LLVMPointerType w) (ps :> LLVMPointerType w) ()
proveVarLLVMArray x ps i off mb_ap =
  error "FIXME HERE NOW"


----------------------------------------------------------------------
-- * Proving Atomic Permissions
----------------------------------------------------------------------

isLLVMFieldMatch :: PermExpr (BVType w) -> AtomicPerm (LLVMPointerType w) ->
                    Bool
isLLVMFieldMatch off (Perm_LLVMField fp) = bvEq off $ llvmFieldOffset fp
isLLVMFieldMatch off (Perm_LLVMArray ap) =
  bvInRange off (llvmArrayRangeBytes ap)
isLLVMFieldMatch _ _ = False

isLLVMArrayMatch :: PermExpr (BVType w) -> AtomicPerm (LLVMPointerType w) ->
                    Bool
isLLVMArrayMatch off (Perm_LLVMArray ap) =
  bvInRange off (llvmArrayRangeBytes ap)
isLLVMArrayMatch _ _ = False

proveVarAtomicImpl :: NuMatchingAny1 r => ExprVar a -> [AtomicPerm a] ->
                      Mb vars (AtomicPerm a) ->
                      ImplM vars r (ps :> a) (ps :> a) ()

proveVarAtomicImpl x ps [nuP| Perm_LLVMField mb_fp |] =
  partialSubstForceM (fmap llvmFieldOffset mb_fp)
  "proveVarPtrPerms: incomplete psubst: LLVM field offset" >>>= \off ->
  case findIndex (isLLVMFieldMatch off) ps of
    Just i -> proveVarLLVMField x ps i off mb_fp
    Nothing ->
      foldr
      (\i rest -> implCatchM (proveVarLLVMField x ps i off mb_fp) rest)
      implFailM
      [0 .. length ps - 1]

proveVarAtomicImpl x ps [nuP| Perm_LLVMArray mb_ap |] =
  partialSubstForceM (fmap llvmArrayOffset mb_ap)
  "proveVarPtrPerms: incomplete psubst: LLVM array offset" >>>= \off ->
  case findIndex (isLLVMArrayMatch off) ps of
    Just i -> proveVarLLVMArray x ps i off mb_ap
    Nothing ->
      foldr
      (\i rest -> implCatchM (proveVarLLVMArray x ps i off mb_ap) rest)
      implFailM
      [0 .. length ps - 1]

proveVarAtomicImpl x ps [nuP| Perm_LLVMFree mb_e |] =
  partialSubstForceM mb_e
  "proveVarAtomicImpl: incomplete psubst: LLVM free size" >>>= \e ->
  case findFreePerm ps of
    Just (i, e') ->
      implCopyConjM x ps i >>> implPopM x (ValPerm_Conj ps) >>>
      castLLVMFreeM x e' e
    _ -> implFailM

proveVarAtomicImpl x [Perm_LLVMFrame
                      fperms] [nuP| Perm_LLVMFrame mb_fperms |] =
  partialSubstForceM mb_fperms
  "proveVarAtomicImpl: incomplete frame perms" >>>= \fperms' ->
  if fperms == fperms' then greturn () else implFailM

proveVarAtomicImpl x [Perm_LOwned ps] [nuP| Perm_LOwned (PExpr_Var z) |]
  | Left memb <- mbNameBoundP z
  = getPSubst >>>= \psubst ->
    case psubstLookup psubst memb of
      Just ps' ->
        if ps == ps' then greturn () else implFailM
      Nothing ->
        setVarM memb ps

proveVarAtomicImpl x [Perm_LOwned ps] [nuP| Perm_LOwned mb_ps |] =
  partialSubstForceM mb_ps
  "proveVarAtomicImpl: incomplete lowned perms" >>>= \ps' ->
  if ps == ps' then greturn () else implFailM

proveVarAtomicImpl x ps p@[nuP| Perm_LCurrent mb_l' |] =
  partialSubstForceM mb_l'
  "proveVarAtomicImpl: incomplete lcurrent perms" >>>= \l' ->
  case ps of
    _ | l' == PExpr_Var x ->
        implPopM x (ValPerm_Conj ps) >>>
        implSimplM Proxy (SImpl_LCurrentRefl x)
    [Perm_LCurrent l] | l == l' -> greturn ()
    [Perm_LCurrent (PExpr_Var l)] ->
      proveVarImpl l (fmap ValPerm_Conj1 p) >>>
      implSimplM Proxy (SImpl_LCurrentTrans x l l')

-- NOTE: existential Perm_Fun vars don't seem to make sense, as they translate
-- to a weird form of polymorphism...
{-
proveVarAtomicImpl x [Perm_Fun fun_perm] [nuP| Perm_Fun (PExpr_Var z) |]
  | Left memb <- mbNameBoundP z
  = getPSubst >>>= \psubst ->
    case psubstLookup psubst memb of
      Just fun_perm'
        | Just Refl <- funPermEq fun_perm fun_perm' -> greturn ()
      Just _ -> implFailM
      Nothing -> setVarM memb fun_perm
-}

proveVarAtomicImpl x [Perm_Fun fun_perm] [nuP| Perm_Fun mb_fun_perm |] =
  partialSubstForceM mb_fun_perm
  "proveVarAtomicImpl: incomplete function perm" >>>= \fun_perm' ->
  case funPermEq fun_perm fun_perm' of
    Just Refl -> greturn ()
    Nothing -> implFailM

proveVarAtomicImpl x ps [nuP| Perm_BVProp mb_prop |] =
  implPopM x (ValPerm_Conj ps) >>>
  partialSubstForceM mb_prop
  "proveVarAtomicImpl: incomplete bitvector proposition" >>>= \prop ->
  implTryProveBVProp x prop

proveVarAtomicImpl _ _ _ = implFailM


-- | Prove @x:(p1 * ... * pn) |- x:(p1' * ... * pm')@ assuming that the LHS
-- conjunction is on the top of the stack, and push any leftover permissions for
-- @x@ back to the primary permissions for @x@
proveVarConjImpl :: NuMatchingAny1 r => ExprVar a -> [AtomicPerm a] ->
                    Mb vars [AtomicPerm a] ->
                    ImplM vars r (ps :> a) (ps :> a) ()
proveVarConjImpl x ps [nuP| [] |] =
  implPopM x (ValPerm_Conj ps) >>> introConjM x
proveVarConjImpl x ps [nuP| mb_p : mb_ps |] =
  proveVarAtomicImpl x ps mb_p >>>
  proveVarImpl x (fmap ValPerm_Conj mb_ps) >>>
  partialSubstForceM mb_p
  "Incomplete psubst: mb_p in proveVarConjImpl" >>>= \p ->
  partialSubstForceM mb_ps
  "Incomplete psubst: mb_ps in proveVarConjImpl" >>>= \ps ->
  implInsertConjM x p ps 0


----------------------------------------------------------------------
-- * Proving Permission Implications
----------------------------------------------------------------------

-- | Prove @x:p'@, where @p@ may have existentially-quantified variables in it
proveVarImpl :: NuMatchingAny1 r => ExprVar a -> Mb vars (ValuePerm a) ->
                ImplM vars r (ps :> a) ps ()
proveVarImpl x mb_p =
  getPerm x >>>= \p ->
  implPushM x p >>>
  proveVarImplH x p mb_p

-- | Prove @x:p'@ assuming that the primary permissions for @x@ have all been
-- pushed to the top of the stack and are equal to @p@. Pop the remaining
-- permissions for @x@ back to its primary permission when we are finished.
proveVarImplH :: NuMatchingAny1 r => ExprVar a -> ValuePerm a ->
                 Mb vars (ValuePerm a) ->
                 ImplM vars r (ps :> a) (ps :> a) ()

-- Prove x:eq(e) by calling proveVarEq; note that we do not eliminate
-- disjunctive permissions first because some trivial equalities do not require
-- any eq permissions on the left
proveVarImplH x p [nuP| ValPerm_Eq e |] = proveVarEq x p e

-- Eliminate any disjunctions and existentials on the left
proveVarImplH x (ValPerm_Or _ _) mb_p =
  elimOrsExistsM x >>>= \p -> proveVarImplH x p mb_p

-- Eliminate any disjunctions and existentials on the left
proveVarImplH x (ValPerm_Exists _) mb_p =
  elimOrsExistsM x >>>= \p -> proveVarImplH x p mb_p

-- Eliminate an equality permissions for a variable on the left, i.e., prove x:p
-- from x:eq(y) by first proving y:p and then casting
proveVarImplH x p@(ValPerm_Eq (PExpr_Var y)) mb_p =
  introEqCopyM x (PExpr_Var y) >>>
  implPopM x p >>>
  proveVarImpl y mb_p >>>
  partialSubstForceM mb_p
  "proveVarImpl: incomplete psubst: introCast" >>>= \p' ->
  introCastM x y p'

-- To prove mu X.p |- mu Y.p', we assume X |- Y and prove p |- p'
--
-- FIXME: we need to somehow ensure that the perms don't change in that proof...
-- FIXME: also need to split the lifetime of the mu if necessary
proveVarImplH x (ValPerm_Mu p) [nuP| ValPerm_Mu mb_p |] =
  error "FIXME HERE NOW: handle mu!"

-- If the LHS is a mu but the RHS is not, unfold the left and recurse
proveVarImplH x (ValPerm_Mu p_body) mb_p =
  implSetMuRecurseLeftM >>> implUnfoldMuM x p_body >>>
  proveVarImplH x (substValPerm (ValPerm_Mu p_body) p_body) mb_p

-- If the RHS is a mu but the LHS is not, unfold the right and recurse
proveVarImplH x p [nuP| ValPerm_Mu mb_p_body |] =
  implSetMuRecurseRightM >>>
  proveVarImplH x p (fmap (\p_body -> substValPerm (ValPerm_Mu p_body) p_body)
                     mb_p_body) >>>
  partialSubstForceM mb_p_body
  "proveVarImpl: incomplete psubst: implFold" >>>= \p_body ->
  implFoldMuM x p_body

-- Prove x:X |- x:Y from an assumption that X |- Y
proveVarImplH _ (ValPerm_Var _) [nuP| ValPerm_Var _ |] =
  error "FIXME HERE NOW: handle permission variables!"

-- Cannot prove x:p |- x:X for any non-variable p
proveVarImplH _ _ [nuP| ValPerm_Var _ |] = implFailM

-- Prove x:(p1 \/ p2) by trying to prove x:p1 and x:p2 in two branches
proveVarImplH x p [nuP| ValPerm_Or mb_p1 mb_p2 |] =
  implCatchM
  (proveVarImplH x p mb_p1 >>>
   partialSubstForceM mb_p1
   "proveVarImpl: incomplete psubst: introOrL" >>>= \p1 ->
    partialSubstForceM mb_p2
   "proveVarImpl: incomplete psubst: introOrL"  >>>= \p2 ->
    introOrLM x p1 p2)
  (proveVarImplH x p mb_p2 >>>
   partialSubstForceM mb_p1
   "proveVarImpl: incomplete psubst: introOrR" >>>= \p1 ->
    partialSubstForceM mb_p2
    "proveVarImpl: incomplete psubst: introOrR"  >>>= \p2 ->
    introOrRM x p1 p2)

-- Prove x:exists (z:tp).p by proving x:p in an extended vars context
proveVarImplH x p [nuP| ValPerm_Exists mb_p |] =
  withExtVarsM (proveVarImplH x p $ mbCombine mb_p) >>>= \((), maybe_e) ->
  let e = fromMaybe (zeroOfType knownRepr) maybe_e in
  partialSubstForceM mb_p "proveVarImpl: incomplete psubst: introExists" >>>=
  introExistsM x e

-- Prove an empty conjunction trivially
proveVarImplH x p [nuP| ValPerm_Conj [] |] = implPopM x p >>> introConjM x

-- Prove x:(p1 * .. * pn) from x:eq(y+off) by proving y:(p1 + off * ...) and
-- then casting the result
proveVarImplH x p@(ValPerm_Eq
                   e@(PExpr_LLVMOffset y off)) mb_p@[nuP| ValPerm_Conj mb_ps |] =
  introEqCopyM x e >>>
  implPopM x p >>>
  proveVarImpl y (fmap (ValPerm_Conj
                        . map (offsetLLVMAtomicPerm off)) mb_ps) >>>
  mapM (flip partialSubstForceM
        "proveVarImpl: incomplete psubst: castLLVMPtr") (mbList mb_ps) >>>= \ps ->
  castLLVMPtrM y ps off x

-- If x:eq(LLVMword(e)) then we cannot prove any pointer permissions for it
proveVarImplH x (ValPerm_Eq (PExpr_LLVMWord _)) [nuP| ValPerm_Conj _ |] =
  implFailM

proveVarImplH x p@(ValPerm_Eq (PExpr_Fun f)) [nuP| ValPerm_Conj
                                                 [Perm_Fun mb_fun_perm] |] =
  (view implStateFnEnv <$> gget) >>>= \env ->
  case lookupFunPerm env f of
    Just (SomeFunPerm fun_perm)
      | [nuP| Just Refl |] <- fmap (funPermEq fun_perm) mb_fun_perm ->
        introEqCopyM x (PExpr_Fun f) >>>
        implPopM x p >>>
        implSimplM Proxy (SImpl_ConstFunPerm x f fun_perm)
    _ -> implFailM

proveVarImplH _ (ValPerm_Eq _) [nuP| ValPerm_Conj _ |] =
  implFailM
  -- FIXME HERE: are there other x:eq(e) |- x:pps cases?

proveVarImplH x (ValPerm_Conj ps) [nuP| ValPerm_Conj mb_ps |] =
  proveVarConjImpl x ps mb_ps

-- Fail if nothing else matched
proveVarImplH _ _ _ = implFailM


-- | A list of distinguished permissions with existential variables
data ExDistPerms vars ps where
  ExDistPermsNil :: ExDistPerms vars RNil
  ExDistPermsCons :: ExDistPerms vars ps -> ExprVar a -> Mb vars (ValuePerm a) ->
                     ExDistPerms vars (ps :> a)

$(mkNuMatching [t| forall vars ps. ExDistPerms vars ps |])

-- | Existentially quantify a list of distinguished permissions over the empty
-- set of existential variables
distPermsToExDistPerms :: DistPerms ps -> ExDistPerms RNil ps
distPermsToExDistPerms DistPermsNil = ExDistPermsNil
distPermsToExDistPerms (DistPermsCons ps x p) =
  ExDistPermsCons (distPermsToExDistPerms ps) x (emptyMb p)

-- | Substitute arguments and a lifetime into a function permission to get the
-- existentially quantified input permissions needed on the arguments
funPermExDistIns :: FunPerm ghosts args ret ->
                    MapRList Name args -> ExprVar LifetimeType ->
                    ExDistPerms ghosts args
funPermExDistIns fun_perm args l =
  error "FIXME HERE NOW"
  {-
  fmap (varSubst (PermVarSubst args) . mbValuePermsToDistPerms
        . varSubst (singletonVarSubst l)) $
  mbSeparate (MNil :>: Proxy) $ funPermIns fun_perm
-}

-- | Prove a list of existentially-quantified distinguished permissions
proveVarsImpl :: NuMatchingAny1 r => ExDistPerms vars as ->
                 ImplM vars r as RNil ()
proveVarsImpl ExDistPermsNil = return ()
proveVarsImpl (ExDistPermsCons ps x p) = proveVarsImpl ps >>> proveVarImpl x p

-- | Like 'proveVarsImpl' but the starting permission set need not be empty, and
-- so is appended to
proveVarsImplAppend :: NuMatchingAny1 r => ExDistPerms vars ps' ->
                       ImplM vars r (ps :++: ps') ps ()
proveVarsImplAppend ExDistPermsNil = return ()
proveVarsImplAppend (ExDistPermsCons ps x p) =
  proveVarsImplAppend ps >>> proveVarImpl x p