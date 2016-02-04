//Question 1
    let rec gcd = function
        | (a,0) -> a
        | (a,b) -> gcd (b, a % b)

    let rec lcm = function
        | (a,0) -> a
        | (a,b) -> ((abs a)/gcd(a,b))*(abs b)

    let simplify (a,b)= 
        let x = gcd (a,b)
        (a/x, b/x)


    let (.+) (a,b) (c,d)=
        let x = lcm(b,d) 
        simplify (a*(x/b) + c*(x/d),x)

    let (.*)  (a,b) (c,d)=
        simplify (a*c,b*d)
//Question 2
