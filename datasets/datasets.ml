open Lib
open Tree

let t1 = 
  Branch(
    'a',
    [
      Branch('b', []);
      Branch('c', []);
      Branch('d', []);
    ]
  )

let t2 = 
  Branch('a',
    [
      Branch('b', 
             [
               Branch('e',[])
             ]);
      Branch('c', []);
      Branch('d', []);
    ])

let t3 =
  Branch('a',
    [
      Branch('b', []);
      Branch('c', 
             [
               Branch('e',[])
             ]);
      Branch('d', []);
    ])

let t4 =
  Branch('a',
    [
      Branch('b', []);
      Branch('c', []);
      Branch('d', 
             [
               Branch('e',[])
             ]);
    ])

let t5 =
  Branch('a',
    [
      Branch('b', []);
      Branch('c', []);
    ])

let t6 = 
  Branch('r', [Branch('f', [Branch('f', [Branch('f',[])])])])

let t7 = 
  Branch('r', [Branch('w',[]); Branch('f', [Branch('c', []); Branch('f', [Branch('f',[])])])])

let t8 = 
  Branch('b', [Branch('c',[])])

let t9 = Branch('a', [t8])
