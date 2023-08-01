use primitive_types::U256;
use crate::evm_utils::{I256};

// Select functions from the evm_core rust library

#[inline]
pub fn exp(op1: U256, op2: U256) -> U256 {
	let mut op1 = op1;
	let mut op2 = op2;
	let mut r: U256 = 1.into();

	while op2 != 0.into() {
		if op2 & 1.into() != 0.into() {
			r = r.overflowing_mul(op1).0;
		}
		op2 >>= 1;
		op1 = op1.overflowing_mul(op1).0;
	}

	r
}

#[inline]
pub fn div(op1: U256, op2: U256) -> U256 {
	if op2 == U256::zero() {
		U256::zero()
	} else {
		op1 / op2
	}
}


#[inline]
pub fn slt(op1: U256, op2: U256) -> U256 {
	let op1: I256 = op1.into();
	let op2: I256 = op2.into();

	if op1.lt(&op2) {
		U256::one()
	} else {
		U256::zero()
	}
}

#[inline]
pub fn sgt(op1: U256, op2: U256) -> U256 {
	let op1: I256 = op1.into();
	let op2: I256 = op2.into();

	if op1.gt(&op2) {
		U256::one()
	} else {
		U256::zero()
	}
}


#[inline]
pub fn not(op1: U256) -> U256 {
	!op1
}

#[inline]
pub fn shl(value: U256, shift: U256) -> U256 {
	if value == U256::zero() || shift >= U256::from(256) {
		U256::zero()
	} else {
		let shift: u64 = shift.as_u64();
		value << shift as usize
	}
}

#[inline]
pub fn shr(value: U256, shift: U256) -> U256 {
	if value == U256::zero() || shift >= U256::from(256) {
		U256::zero()
	} else {
		let shift: u64 = shift.as_u64();
		value >> shift as usize
	}
}

