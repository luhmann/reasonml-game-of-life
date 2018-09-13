let foo = [|1, 2, 3|];

print_int(Array.fold_left((acc, item) => acc + item, 0, foo));
