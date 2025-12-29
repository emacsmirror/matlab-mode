% -*- matlab-ts -*-

a0 = [1; 1];

a1 =   [   [  1 ]; [  7  ]];

a2 =   [   [  1, 3, (4+4)*2]    ; [  5,6, 7  ]];

b =    [[1,3,4];[5,6,7]]

c =    [1  , 2  ];

d=[0     0];

e=0

e+e

[-e    +e]

f = [e + e -e +e]

g = - e

   h =      [1 2; ...
       3    4   ];

c2 = {  ['freq'   '%']   num2str(2)};

c3 = {a b [c '%'] f(1)};

x = [
      1, 2;
      3, 4;
    ];

x2 = [
      1  ,   2  ;
           3   ,4  ;
    ];


d1 = dictionary([nativeProperties(:,1);propertyStateArray(:,1)], ...
            [nativeProperties(:,2);propertyStateArray(:,2)]);

d2 = dictionary([nativeProperties(:, 1); propertyStateArray(:, 1)], ...
                [nativeProperties(:, 2); propertyStateArray(:, 2)]);

m3 = uint8(  [  ones(20,1);   2*ones(8,1);ones(8,1);    2*ones(8,1); ones(7,1)    ]);

foo123(t0,:) = tan(th(t0)/2).*[sx(t0)' sy(t0)' sz(t0)'];

c3 = {{[17.50 0] [17.50 0]} {[120 0] [120 20]}};


c4{1} = [1 2; 3 4];
v4 = [c4{1}(1,  1), c4{1}(1,   1)];
