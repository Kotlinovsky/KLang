let main = lambda x { 
    let factorial = lambda cur, res {
        if cur==x {
            return res;
        };
        let next = cur+1;
        let nextres = res*next;
        let finalres = factorial(next, nextres);
        return finalres;
    };
    let calculated = factorial(0, 1);
    print(calculated);
}