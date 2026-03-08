/// An interval representing the possible range of an integer value.
///
/// Used by the array bounds checker for abstract interpretation.
/// The interval `[min, max]` represents all integers `x` where `min <= x <= max`.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct IndexInterval {
    pub min: i64,
    pub max: i64,
    /// True if the value is known to be non-zero.
    pub non_zero: bool,
}

impl std::ops::Add<IndexInterval> for IndexInterval {
    type Output = IndexInterval;

    fn add(self, rhs: IndexInterval) -> IndexInterval {
        // Check for overflow in min calculation
        let min = if let Some(result) = self.min.checked_add(rhs.min) {
            result
        } else {
            // Overflow occurred, return saturated value
            if self.min > 0 && rhs.min > 0 {
                i64::MAX
            } else {
                i64::MIN
            }
        };

        // Check for overflow in max calculation
        let max = if let Some(result) = self.max.checked_add(rhs.max) {
            result
        } else {
            // Overflow occurred, return saturated value
            if self.max > 0 && rhs.max > 0 {
                i64::MAX
            } else {
                i64::MIN
            }
        };

        IndexInterval { min, max, non_zero: false }
    }
}

impl std::ops::Sub<IndexInterval> for IndexInterval {
    type Output = IndexInterval;

    fn sub(self, rhs: IndexInterval) -> IndexInterval {
        // For subtraction: [a, b] - [c, d] = [a - d, b - c]
        // The minimum result is self.min - rhs.max
        // The maximum result is self.max - rhs.min
        let min = if let Some(result) = self.min.checked_sub(rhs.max) {
            result
        } else {
            // Overflow occurred, return saturated value
            if self.min < 0 && rhs.max > 0 {
                i64::MIN
            } else {
                i64::MAX
            }
        };

        let max = if let Some(result) = self.max.checked_sub(rhs.min) {
            result
        } else {
            // Overflow occurred, return saturated value
            if self.max > 0 && rhs.min < 0 {
                i64::MAX
            } else {
                i64::MIN
            }
        };

        IndexInterval { min, max, non_zero: false }
    }
}

impl std::ops::Mul<IndexInterval> for IndexInterval {
    type Output = IndexInterval;

    fn mul(self, rhs: IndexInterval) -> IndexInterval {
        // For multiplication: [a, b] * [c, d]
        // We need to consider all four products and take min/max
        // because signs matter: e.g., [-2, 3] * [1, 4] = [-8, 12]
        let products = [
            self.min.saturating_mul(rhs.min),
            self.min.saturating_mul(rhs.max),
            self.max.saturating_mul(rhs.min),
            self.max.saturating_mul(rhs.max),
        ];

        IndexInterval {
            min: *products.iter().min().unwrap(),
            max: *products.iter().max().unwrap(),
            non_zero: false,
        }
    }
}

impl IndexInterval {
    /// Returns true if this interval provably excludes zero.
    pub fn excludes_zero(&self) -> bool {
        self.non_zero || self.min > 0 || self.max < 0
    }
}

impl Default for IndexInterval {
    fn default() -> IndexInterval {
        IndexInterval {
            min: i64::MIN,
            max: i64::MAX,
            non_zero: false,
        }
    }
}

/// Encloses two index intervals, returning the smallest interval that contains both.
pub fn enclose(a: IndexInterval, b: IndexInterval) -> IndexInterval {
    IndexInterval {
        min: a.min.min(b.min),
        max: a.max.max(b.max),
        non_zero: a.non_zero && b.non_zero,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index_interval_add() {
        let a = IndexInterval { min: 0, max: 10, non_zero: false };
        let b = IndexInterval { min: 5, max: 15, non_zero: false };
        let c = a + b;
        assert_eq!(c.min, 5);
        assert_eq!(c.max, 25);

        let d = IndexInterval::default();
        let e = a + d;
        assert_eq!(e.min, i64::MIN);
        assert_eq!(e.max, i64::MAX);
    }

    #[test]
    fn test_index_interval_sub() {
        // [0, 10] - [5, 15] = [0 - 15, 10 - 5] = [-15, 5]
        let a = IndexInterval { min: 0, max: 10, non_zero: false };
        let b = IndexInterval { min: 5, max: 15, non_zero: false };
        let c = a - b;
        assert_eq!(c.min, -15);
        assert_eq!(c.max, 5);

        // [10, 20] - [2, 3] = [10 - 3, 20 - 2] = [7, 18]
        let d = IndexInterval { min: 10, max: 20, non_zero: false };
        let e = IndexInterval { min: 2, max: 3, non_zero: false };
        let f = d - e;
        assert_eq!(f.min, 7);
        assert_eq!(f.max, 18);
    }

    #[test]
    fn test_index_interval_mul() {
        // [2, 3] * [4, 5] = [8, 15]
        let a = IndexInterval { min: 2, max: 3, non_zero: false };
        let b = IndexInterval { min: 4, max: 5, non_zero: false };
        let c = a * b;
        assert_eq!(c.min, 8);
        assert_eq!(c.max, 15);

        // [-2, 3] * [1, 4] = [-8, 12]
        let d = IndexInterval { min: -2, max: 3, non_zero: false };
        let e = IndexInterval { min: 1, max: 4, non_zero: false };
        let f = d * e;
        assert_eq!(f.min, -8);
        assert_eq!(f.max, 12);

        // [-3, -1] * [-2, 4] = [-12, 6]
        let g = IndexInterval { min: -3, max: -1, non_zero: false };
        let h = IndexInterval { min: -2, max: 4, non_zero: false };
        let i = g * h;
        assert_eq!(i.min, -12);
        assert_eq!(i.max, 6);
    }

    #[test]
    fn test_enclose() {
        let a = IndexInterval { min: 0, max: 10, non_zero: false };
        let b = IndexInterval { min: 5, max: 15, non_zero: false };
        let c = enclose(a, b);
        assert_eq!(c.min, 0);
        assert_eq!(c.max, 15);
    }
}
