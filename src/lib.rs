use core::ops::{BitAnd};
mod evm_ops;
pub mod evm_utils;
use egg::{define_language, Id, Symbol};
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


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constant {
    Bool(bool),
    Num(U256),
}


impl fmt::Display for Constant {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self {
          Constant::Bool(b) => b.fmt(f),
          Constant::Num(i) => i.fmt(f),
      }
  }
}

impl std::str::FromStr for Constant {
  type Err = ();
  fn from_str(s: &str) -> Result<Self, Self::Err> {
      if let Ok(i) = U256::from_dec_str(s) {
          Ok(Self::Num(i))
      } else if let Ok(b) = bool::from_str(s) {
          Ok(Self::Bool(b))
      } else {
          Err(())
      }
  }
}

impl Constant {
  fn to_num(&self) -> Option<U256> {
      match self {
          Constant::Num(n) => Some(n.clone()),
          Constant::Bool(_) => None,
      }
  }

  fn to_bool(&self) -> Option<bool> {
      match self {
          Constant::Bool(b) => Some(*b),
          Constant::Num(_) => None,
      }
  }
}



#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoolVar(pub egg::Symbol);

impl Display for BoolVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let BoolVar(v) = self;
    write!(f, "{}", v)
  }
}

impl FromStr for BoolVar {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("bool") {
            Ok(BoolVar(Symbol::from(s)))
        } else {
            Err(())
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BitVar(pub egg::Symbol);

impl Display for BitVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let BitVar(v) = self;
    write!(f, "{}", v)
  }
}


impl FromStr for BitVar {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("bit256") {
            Ok(BitVar(Symbol::from(s)))
        } else {
            Err(())
        }
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
      "bool==" = BoolEq([Id; 2]),
      "bit==" = BitEq([Id; 2]),
      "s<" = Slt([Id; 2]),

      "s<=" = Sle([Id; 2]),
      "s>" = Sgt([Id; 2]),
      "s>=" = Sge([Id; 2]),

      "+" = Add([Id; 2]),
      "*" = Mul([Id; 2]),

      "!" = LNot([Id; 1]),
      "~" = BWNot([Id; 1]),

      "exp" = Exp([Id; 2]),

      "bitif" = BitIte([Id; 3]),
      "boolif" = BoolIte([Id; 3]),

      Constant(Constant),
      BoolVar(BoolVar),
      BitVar(BitVar),
  }
}

impl EVM {
  pub fn new(n: U256) -> Self {
    EVM::Constant(Constant::Num(n))
  }

  pub fn from_u64(n: u64) -> Self {
    EVM::Constant(Constant::Num(U256::from_dec_str(&n.to_string()).unwrap()))
  }
}

impl From<U256> for EVM {
  fn from(t: U256) -> Self {
      Self::new(t)
  }
}

impl From<bool> for EVM {
  fn from(t: bool) -> Self {
    EVM::Constant(Constant::Bool(t))
  }
}

impl From<Constant> for EVM {
  fn from(t: Constant) -> Self {
    EVM::Constant(t)
  }
}

// This function should only return None for variables
// and when we get the wrong number of arguments
pub fn eval_evm(
  op: &EVM,
  first: Option<Constant>,
  second: Option<Constant>,
  third: Option<Constant>,
) -> Option<Constant> {
  Some(match op {
      EVM::Constant(c) => c.clone(),
      EVM::BitVar(_) => None?,
      EVM::BoolVar(_) => None?,

      EVM::Sub(_) => Constant::Num(first?.to_num()?.overflowing_sub(second?.to_num()?).0),
      EVM::Div(_) => Constant::Num(evm_ops::div(first?.to_num()?, second?.to_num()?)),
      EVM::BWAnd(_) => Constant::Num(first?.to_num()?.bitand(second?.to_num()?)),
      EVM::BWOr(_) => Constant::Num(first?.to_num()?.bitor(second?.to_num()?)),
      EVM::ShiftLeft(_) => Constant::Num(evm_ops::shl(first?.to_num()?, second?.to_num()?)),
      EVM::ShiftRight(_) => Constant::Num(evm_ops::shr(first?.to_num()?, second?.to_num()?)),

      EVM::LOr(_) => Constant::Bool(first?.to_bool()? || second?.to_bool()?),
      EVM::LAnd(_) => Constant::Bool(first?.to_bool()? && second?.to_bool()?),

      EVM::Gt(_) => Constant::Bool(first?.to_num()?.gt(&second?.to_num()?)),
      EVM::Ge(_) => Constant::Bool(first?.to_num()?.ge(&second?.to_num()?)),
      EVM::Lt(_) => Constant::Bool(first?.to_num()?.lt(&second?.to_num()?)),
      EVM::Le(_) => Constant::Bool(first?.to_num()?.le(&second?.to_num()?)),
      EVM::BoolEq(_) => Constant::Bool(first?.eq(&second?)),
      EVM::BitEq(_) => Constant::Bool(first?.eq(&second?)),
      

      EVM::Slt(_) => Constant::Bool(evm_ops::slt(first?.to_num()?, second?.to_num()?) == U256::one()),
      EVM::Sle(_) => Constant::Bool(if first.clone()?.to_num()? == second.clone()?.to_num()? { true } 
      else
      {evm_ops::slt(first?.to_num()?, second?.to_num()?) == U256::one()}),
      EVM::Sgt(_) => Constant::Bool(evm_ops::sgt(first?.to_num()?, second?.to_num()?) == U256::one()),
      EVM::Sge(_) => Constant::Bool(if first.clone()?.to_num()? == second.clone()?.to_num()? { true } else {evm_ops::sgt(first?.to_num()?, second?.to_num()?) == U256::one()}),

      EVM::Add(_) => Constant::Num(first?.to_num()?.overflowing_add(second?.to_num()?).0),
      EVM::Mul(_) => Constant::Num(first?.to_num()?.overflowing_mul(second?.to_num()?).0),

      EVM::LNot(_) => Constant::Bool(!first?.to_bool()?),
      EVM::BWNot(_) => Constant::Num(evm_ops::not(first?.to_num()?)),
      EVM::Exp(_) => Constant::Num(evm_ops::exp(first?.to_num()?, second?.to_num()?)),

      EVM::BitIte(_) => Constant::Num(if first?.to_bool()? { second?.to_num()? } else { third?.to_num()? }),
      EVM::BoolIte(_) => Constant::Bool(if first?.to_bool()? { second?.to_bool()? } else { third?.to_bool()? }),
  })
}
