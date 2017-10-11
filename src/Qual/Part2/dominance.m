function flag = dominance(a,b)
    flag = 0;
    flag = ((a(1) + a(3)) >= (b(1) + b(3))) && (a(2) <= b(2));
end