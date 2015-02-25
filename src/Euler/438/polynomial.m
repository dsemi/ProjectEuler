c = 0;
for a1=-14:-10
    for a2=35:71
        for a3=-154:-50
            for a4=24:120
            r = roots([1 a1 a2 a3 a4]);
                if r(4)>=1 && r(4) < 2 && r(3)>=2 && r(3) < 3 && r(2)>=3 && r(2) < 4 && r(1)>=4 && r(1) < 5
                    c = c + abs(a1) + abs(a2) + abs(a3) + abs(a4);
                    r
                end
            end
        end
    end
end
