use std::collections::HashMap;
use std::f32;

const ENG_FREQ:[(char, f32); 26] = [
    ('E', 0.12019549870270922),
    ('T', 0.09098588613462202),
    ('A', 0.08123837786542185),
    ('O', 0.07681168165087793),
    ('I', 0.07305420097310522),
    ('N', 0.06947773761265585),
    ('S', 0.06280752373795274),
    ('R', 0.06021294218965129),
    ('H', 0.05921460425774672),
    ('D', 0.043191828988003486),
    ('L', 0.03978541219837304),
    ('U', 0.02877626808116158),
    ('C', 0.027114199985738028),
    ('M', 0.02611586205383345),
    ('F', 0.02303856765933638),
    ('Y', 0.021135143140815018),
    ('W', 0.020948640450239437),
    ('G', 0.020257483420459344),
    ('P', 0.018189497704371293),
    ('B', 0.014892788379785303),
    ('V', 0.011074968596238131),
    ('K', 0.006895114178044245),
    ('X', 0.0017278925744502285),
    ('Q', 0.0011245015167057042),
    ('J', 0.0010312501714179142),
    ('Z', 0.0007021277762845373)
];

//

pub fn xor_fixed(buffer1: Vec<u8>, buffer2: Vec<u8>) -> Vec<u8> {
    buffer1.into_iter()
        .zip(buffer2.into_iter())
        .map(|(a, b)| a ^ b)
        .collect()
}

pub fn xor_byte(buffer1: &Vec<u8>, byte: u8) -> Vec<u8> {
    buffer1.iter()
        .map(|x| x ^ byte)
        .collect()
}

pub fn eng_similarity(buffer: &Vec<u8>) -> f32 {
    let mut count: HashMap<u8, u32> = HashMap::new();

    // keep track of all A-Z
    for c in ('A' as u8)..('Z' as u8 + 1) {
        count.insert(c, 0);
    }

    // count all letters
    let mut total_count = 0;
    for b in buffer {
        let b = (*b as char).to_ascii_uppercase() as u8;
        if count.contains_key(&b) {
            *count.get_mut(&b).unwrap() += 1;
            total_count += 1;
        }
    }

    let total_count = total_count as f32;

    if total_count == 0f32 {
        return f32::INFINITY;
    }

    // compute similarity score
    let mut score: f32 = 0.0;
    for (c, pi) in &ENG_FREQ {
        let c = *c;
        let pi = *pi;
            
        let qi = *count.get(&(c as u8)).unwrap();
        let qi = (qi as f32) / total_count;

        let diff = (qi - pi).abs();
        score += diff.ln();
    }

    let frac = total_count / (buffer.len() as f32);
    score * frac
}

pub fn decrypt_xor_byte(buffer1: &Vec<u8>) -> u8 {
    (0..255).into_iter()
        .map(|x| {
            (x, x)
        })
        .map(|(i, x)| (i, xor_byte(buffer1, x)))
        .map(|(i, b)| (i, eng_similarity(&b)))
        .fold((0, f32::INFINITY), |(i, v), (ni, nv)| {
            if nv < v {
                return (ni, nv)
            } else {
                return (i, v)
            }
        }).0
}

#[cfg(test)]
mod lib_test {
    use super::*;
    use convert::*;

    #[test]
    pub fn test_xor_fixed() {
        let buffer1 = hex_decode("1c0111001f010100061a024b53535009181c".to_string());
        let buffer2 = hex_decode("686974207468652062756c6c277320657965".to_string());
        let out = xor_fixed(buffer1, buffer2);
        let out = hex_encode(out);
        assert_eq!(out, "746865206b696420646f6e277420706c6179");
    }
}
