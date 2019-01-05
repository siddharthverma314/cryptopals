pub struct NMin<T> where T: PartialOrd + Copy {
    mins: Vec<Option<T>>,
}

impl<T> NMin<T> where T: PartialOrd + Copy {
    pub fn new(size: usize) -> NMin<T> {
        let mut vec = Vec::with_capacity(size);
        for _ in 0..size {
            vec.push(None);
        }
        NMin{mins: vec}
    }

    pub fn get_items(self) -> Vec<T> {
        let mut res: Vec<T> = Vec::with_capacity(self.mins.len());
        for elem in self.mins.into_iter(){
            if let Some(i) = elem {
                res.push(i);
            }
        }
        res
    }

    pub fn update(&mut self, elem: T) {
        for pos in 0..self.mins.len() {
            let update = match self.mins.get(pos).unwrap() {
                None => true,
                Some(t) => elem < *t
            };

            if update {
                self.insert(elem, pos);
                break;
            }
        }
    }

    fn insert(&mut self, elem: T, pos: usize) {
        self.mins.pop();
        self.mins.insert(pos, Some(elem));
    }
}

#[cfg(test)]
mod test {
    extern crate rand;
    use super::*;

    #[test]
    fn simple_test_nmin() {
        let mut nmin: NMin<i32> = NMin::new(3);

        for i in vec![3, 1, 2, 6, 5, 4, 7, 6, 8, 9].into_iter() {
            nmin.update(i);
        }

        assert_eq!(nmin.get_items(), vec![1, 2, 3]);
    }

    #[test]
    fn test_random_nmin() {
        const NUM_ITER: usize = 100;
        const NUM_ELEM_VEC: usize = 1000;
        const NUM_MIN_SIZE: usize = 15;

        for _ in 0..NUM_ITER {
            let mut nmin: NMin<i32> = NMin::new(NUM_MIN_SIZE);
            let mut vec = Vec::with_capacity(NUM_ELEM_VEC);

            for _ in 0..NUM_ELEM_VEC {
                let i = rand::random::<i32>();
                vec.push(i);
                nmin.update(i);
            }

            vec.sort();

            assert_eq!(nmin.get_items(), &vec[0..NUM_MIN_SIZE]);
        }
    }

    #[test]
    fn test_tuples() {
        assert!((1, 2) < (2, 1));
        assert!((2, 1) > (1, 3));
    }
}
