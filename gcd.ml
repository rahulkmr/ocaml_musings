let rec gcd a b =
  if b > a then gcd b a else
    let r = a mod b in
    if r = 0 then b else gcd b r

