use core::ops::{BitAnd};
use std::hash::Hash;
mod evm_ops;
pub mod evm_utils;
use egg::{define_language, Id};
use primitive_types::U256;
use std::ops::*;

use std::str::FromStr;
use std::fmt::{self, Display};

#[derive(Debug, Clone, PartialOrd, Ord, Eq, PartialEq, Hash)]
pub struct WrappedU256 {
    pub value: U256,
}

impl FromStr for WrappedU256 {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match U256::from_dec_str(s) {
            Ok(v) => Ok(WrappedU256 { value: v }),
            Err(_) => Err(()),
        }
    }
}

impl Display for WrappedU256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.value)
    }
}



define_language! {
  pub enum EVM {
      "-" = Sub([Id; 2]),
      "/" = Div([Id; 2]),
      "&" = BWAnd([Id; 2]),
      "|" = BWOr([Id; 2]),
      "<<" = ShiftLeft([Id; 2]),
      ">>" = ShiftRight([Id; 2]),
      "||" = LOr([Id; 2]),
      "&&" = LAnd([Id; 2]),

      ">" = Gt([Id; 2]),
      ">=" = Ge([Id; 2]),
      "<" = Lt([Id; 2]),
      "<=" = Le([Id; 2]),
      "==" = Eq([Id; 2]),
      "s<" = Slt([Id; 2]),
      "s<=" = Sle([Id; 2]),
      "s>" = Sgt([Id; 2]),
      "s>=" = Sge([Id; 2]),

      "+" = Add([Id; 2]),
      "*" = Mul([Id; 2]),

      "!" = LNot([Id; 1]),
      "~" = BWNot([Id; 1]),
      "exp" = Exp([Id; 2]),

      "apply" = Apply(Box<[Id]>),

      "safe_math_narrow" = SafeMathNarrow(Box<[Id]>),
      "safe_math_promotion" = SafeMathPromotion(Box<[Id]>),
      "smul_noofl" = SmulNoofl(Box<[Id]>),
      "smul_noudfl" = SmulNoudfl(Box<[Id]>), 
      "smul_no_ofl_udfl" = SmulNoOflUdfl(Box<[Id]>),
      "hash" = Hash(Box<[Id]>),
      "skey_add" = SkeyAdd(Box<[Id]>),
      "skey_basic" = SkeyBasic(Box<[Id]>),
      "unwrap_twos_complement" = UnwrapTwosComplement(Box<[Id]>),
      "wrap_twos_complement" = WrapTwosComplement(Box<[Id]>),
      "ecrecover" = Ecrecover(Box<[Id]>),
      "opaque_identity" = OpaqueIdentity(Box<[Id]>),
      "link_library" = LinkLibrary(Box<[Id]>),
      "to_storage_key" = ToStorageKey(Box<[Id]>),
      "add_noofl" = AddNoofl(Box<[Id]>),
      "add_must_ofl" = AddMustOfl(Box<[Id]>),
      "mul_noofl" = MulNoofl(Box<[Id]>),
      "disjoint_sighashes" = DisjointSighashes(Box<[Id]>),
      "from_skey" = FromSkey(Box<[Id]>),
      "to_skey" = ToSkey(Box<[Id]>),

      Num(WrappedU256),
      Var(egg::Symbol),
  }
}

impl EVM {
  pub fn new(n: U256) -> Self {
      EVM::Num(WrappedU256 { value: n })
  }

  pub fn from_u64(n: u64) -> Self {
      EVM::Num(WrappedU256 { value : U256::from_dec_str(&n.to_string()).unwrap() })
  }
}

impl From<U256> for EVM {
  fn from(t: U256) -> Self {
      Self::new(t)
  }
}

fn u256_to_bool(u: U256) -> bool {
  u != U256::zero()
}

fn bool_to_u256(b: bool) -> U256 {
  if b {
      U256::one()
  } else {
      U256::zero()
  }   
}


// This function should only return None for variables
// and when we get the wrong number of arguments
pub fn eval_evm(
  op: &EVM,
  first: Option<U256>,
  second: Option<U256>,
) -> Option<U256> {
  Some(match op {
      EVM::Num(n) => n.value,
      EVM::Var(_) => None?,

      EVM::Sub(_) => first?.overflowing_sub(second?).0,
      EVM::Div(_) => evm_ops::div(first?, second?),
      EVM::BWAnd(_) => first?.bitor(second?),
      EVM::BWOr(_) => first?.bitand(second?),
      EVM::ShiftLeft(_) => evm_ops::shl(first?, second?),
      EVM::ShiftRight(_) => evm_ops::shr(first?, second?),

      EVM::LOr(_) => bool_to_u256(u256_to_bool(first?) || u256_to_bool(second?)),
      EVM::LAnd(_) => bool_to_u256(u256_to_bool(first?) && u256_to_bool(second?)),

      EVM::Gt(_) => bool_to_u256(first?.gt(&second?)),
      EVM::Ge(_) => bool_to_u256(first?.ge(&second?)),
      EVM::Lt(_) => bool_to_u256(first?.lt(&second?)),
      EVM::Le(_) => bool_to_u256(first?.le(&second?)),
      EVM::Eq(_) => bool_to_u256(first?.eq(&second?)),

      EVM::Slt(_) => evm_ops::slt(first?, second?),
      EVM::Sle(_) => if first? == second? { bool_to_u256(true) } else {evm_ops::slt(first?, second?)},
      EVM::Sgt(_) => evm_ops::sgt(first?, second?),
      EVM::Sge(_) => if first? == second? { bool_to_u256(true) } else {evm_ops::sgt(first?, second?)},

      EVM::Add(_) => first?.overflowing_add(second?).0,
      EVM::Mul(_) => first?.overflowing_mul(second?).0,

      EVM::LNot(_) => bool_to_u256(!u256_to_bool(first?)),
      EVM::BWNot(_) => evm_ops::not(first?),
      EVM::Exp(_) => evm_ops::exp(first?, second?),
      EVM::Apply(_) => None?,

      EVM::SafeMathNarrow(_) => None?,
      EVM::SafeMathPromotion(_) => None?,
      EVM::SmulNoOflUdfl(_) => None?,
      EVM::SmulNoofl(_) => None?,
      EVM::SmulNoudfl(_) => None?,
      EVM::DisjointSighashes(_) => None?,
      EVM::MulNoofl(_) => None?,
      EVM::AddMustOfl(_) => None?,
      EVM::AddNoofl(_) => None?,
      EVM::SkeyAdd(_) => None?,
      EVM::SkeyBasic(_) => None?,
      EVM::Hash(_) => None?,
      EVM::LinkLibrary(_) => None?,
      EVM::OpaqueIdentity(_) => None?,
      EVM::Ecrecover(_) => None?,
      EVM::WrapTwosComplement(_) => None?,
      EVM::UnwrapTwosComplement(_) => None?,
      EVM::ToSkey(_) => None?,
      EVM::FromSkey(_) => None?,
      EVM::ToStorageKey(_) => None?,

  })
}
