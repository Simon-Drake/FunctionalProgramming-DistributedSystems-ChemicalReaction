-module ( chemistry ).
-export ([ printfinal/1, react/2, begin_react/2, super_react/2, collide/4, hco/0, h/0, ch3o/0, ch2oh/0, co/0, c/0, ch3/0, ch4/0, o/0, h2/0, oh/0, ch3oh/0, o2/0]).


% Define the processes for all the intermediary molecules. 
h2() ->
    receive 
        {input, o} ->
            io: format ("H2 = o.H2O ~n", []);
        {input, oh} ->
            io: format ("H2 = oh.(H2O|H) ~n", []);
        {output, h} ->
            io: format ("H2 = ~~h.H ~n", [])
    end.

co() ->
    receive 
        {input, o} ->
            io: format ("CO = o.CO2 ~n", []);
        {input, h} ->
            io: format ("CO = h.HCO ~n", [])
    end.

h() ->
    receive 
        {output,h} ->
            io: format ("H = ~~h.0  ~n", [])                
    end.


c() ->
    receive 
        {input,o} ->
            io: format ("C = o.CO  ~n", []);
        {output,c} ->
            io: format ("C = ~~c.0  ~n", [])   
    end.

oh() ->
    receive 
        {output, oh} ->
            io: format ("OH = ~~oh.0  ~n", []);
        {output,h} ->
            io: format ("OH = ~~h.H  ~n", []);
        {input,h} ->
            io: format ("OH = h.H2O  ~n", []) 
    end.

ch3() ->
    receive 
        {input, h} ->
            io: format ("CH3 = h.CH4 ~n", []);
        {input, o} ->
            io: format ("CH3 = o.CH3O ~n", [])
    end.

ch4() ->
    receive 
        {input, oh} ->
            io: format ("CH4 = oh.(H2O|CH3) ~n", []);
        {input, o} ->
            io: format ("CH4 = o.(CH3O|H) ~n", [])
    end.

hco() ->
    receive 
        {input, o} ->
            io: format ("HCO = o.(CO2|H) ~n", []);
        {input, h} ->
            io: format ("HCO = h.(C|H2O) ~n", []);
        {output, c} ->
            io: format ("HCO = ~~c.(OH) ~n", [])
    end.

o() ->
    receive 
        {output, o} ->
            io: format ("O = ~~o.0  ~n", []);
        {input, h} ->
            io: format ("O = h.OH  ~n", [])
    end.

o2() ->
    receive           
        {output, o} ->
            io: format ("O2 = ~~o.O  ~n", []);
        {input, c} ->
            io: format ("O2 = c.(CO2)  ~n", []);
        {input, h} ->
            io: format ("O2 = h.(OH|O)  ~n", [])
    end.

ch3oh() ->
    receive           
        {input,o} ->
            io: format ("CH3OH = o.(HCO|OH|H|H) ~n", []);
        {input,h} ->
            io: format ("CH3OH = h.(CH2OH|H2) ~n", []);
        {input, oh} ->
            io: format ("CH3OH = oh.(CH3|O|H2O) ~n", [])
    end.

ch3o() ->
    receive           
        {input, h} ->
            io: format ("CH3O = h.(CH4|O) ~n", []);
        {input, oh} ->
            io: format ("CH3O = oh.(HCO|H|H2O) ~n", [])
    end.

ch2oh() ->
    receive           
        {input, o} ->
            io: format ("CH2OH = o.(HCO|H2O) ~n", []);
        {input, h} ->
            io: format ("CH2OH = h.(CH4|O) ~n", [])
    end.

% Calling function checks correct input
react (M,O) ->
    if
        M =< 0 -> 
            io: format ("You need to pass a positive integer of methanol and oxygen molecules ~n", []);
        M > 0 ->
            if
                O =< 0 -> 
                    io: format ("You need to pass a positive integer of methanol and oxygen molecules ~n", []);
                O > 0 -> 
                    begin_react(M,O)
                end
        end.

% Function to call with number of methanol and oxygen molecules. 
% Initialises lists of processes.

begin_react (M,O) ->
    Z = O rem M,
    D = O - M,
    if
        O =< M ->
            S = " There is not enough oxygen for full combustion ";
        D /= Z ->
            S = " The equation does not balance there will be left over molecules which are not H2O and CO2 ";
        D == Z ->
            S = " All the methanol fully combusted "
        end,
    
    CH3OHP = [],
    CH2OHP = [],
    CH4P = [],
    CH3OP = [],
    HCOP = [],
    CH3P = [],
    H2P = [],
    O2P = [],
    OP = [],
    OHP = [],
    HP = [],
    CP = [],
    COP = [],
    super_react({[M,O], {CH3OHP, CH2OHP, CH4P, CH3OP, HCOP, CH3P,H2P,O2P,OP,OHP,HP,CP,COP}}, S).


% This function consists of translating all of the molecules given as input to processes 
super_react(X, S) ->
    case X of 
        {[0, 0],P} -> 
            collide(P, 0 ,0, S);

        {[M,0],{CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,OP,OHP,HP,CP,COP}} -> 
            Mx_id = spawn (chemistry, ch3oh, [  ]  ),
            super_react({[M-1, 0],  {CH3OHP++[Mx_id],CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,OP,OHP,HP,CP,COP}}, S);

        {[0,O],{CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,OP,OHP,HP,CP,COP}} -> 
            O2x_id = spawn (chemistry,  o2, [  ]  ),
            super_react({[0, O-1],  {CH3OHP,CH2OHP, CH4P, CH3OP, HCOP, CH3P,H2P,O2P++[O2x_id],OP,OHP,HP,CP,COP}}, S);

        {[M, O],{CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,OP,OHP,HP,CP,COP}} -> 
                O2x_id = spawn (chemistry,  o2, [ ]  ),
                Mx_id = spawn (chemistry, ch3oh, [  ] ),
                super_react({[M-1, O-1],  {CH3OHP++[Mx_id],CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P++[O2x_id],OP,OHP,HP,CP,COP}}, S)
        end.

% This function deals with all the reactions
collide(P, H2O , CO2, S) ->
    case P of
        {[],[],[],[],[],[],[],[],[],[],[],[],[]} ->
            io: format ("~n~n****Reaction completed with ~p H2O molecules and ~p CO2 molecules****~n", [H2O, CO2]);

        {CH3OHP,[CH2OHP|TCH2OH], CH4P, CH3OP, HCOP,CH3P,H2P,O2P,[OP|TO],OHP,HP,CP,COP} ->

            OP ! {output, o},
            CH2OHP ! {input, o},

            io: format ("~n**chemical equation** CH2OH + O -> HCO + H2O ~n", []),

            HCOx_id = spawn (chemistry, hco, [ ]  ),

            collide({CH3OHP,TCH2OH,CH4P,CH3OP,HCOP++[HCOx_id],CH3P,H2P,O2P,TO,OHP,HP,CP,COP}, H2O+1, CO2, S);

        {CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,OP,[OHP|TOH],HP,[CP|TCP],COP} ->

            CP ! {input, o},
            OHP ! {output,h},

            io: format ("~n**chemical equation** C + OH -> CO + H ~n", []),

            COx_id = spawn (chemistry, co, []  ),
            Hx_id = spawn (chemistry, h, [ ]  ),

            collide({CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,OP,TOH,HP++[Hx_id],TCP,COP++[COx_id]}, H2O, CO2, S );

        {CH3OHP,CH2OHP, CH4P, CH3OP,[HCOP|THCO],CH3P,H2P,O2P,[OP|TO],OHP,HP,CP,COP} ->

            OP ! {output, o},
            HCOP ! {input, o},

            io: format ("~n**chemical equation** HCO + O -> CO2 + H ~n", []),

            Hx_id = spawn (chemistry, h, [ ]  ),

            collide({CH3OHP,CH2OHP,CH4P,CH3OP,THCO,CH3P,H2P,O2P,TO,OHP,HP++[Hx_id],CP,COP}, H2O, CO2 + 1, S);

        {CH3OHP,CH2OHP, CH4P, CH3OP,HCOP,CH3P,[H2P|TH2],O2P,OP,OHP,HP,CP,[COP|TCO]} ->

            H2P ! {output, h},
            COP ! {input, h},

            io: format ("~n**chemical equation** CO + H2 -> HCO + H ~n", []),

            HCOx_id = spawn (chemistry, hco, [ ]  ),
            Hx_id = spawn (chemistry, hco, [ ]  ),

            collide({CH3OHP,CH2OHP,CH4P,CH3OP,HCOP++[HCOx_id],CH3P,TH2,O2P,OP,OHP,HP++[Hx_id],CP,TCO}, H2O, CO2, S);

        {CH3OHP,CH2OHP, CH4P, CH3OP,[HCOP|THCO],CH3P,H2P,O2P,OP,OHP,[HP|TH],CP,COP} ->

            HP ! {output, h},
            HCOP ! {input, h},

            io: format ("~n**chemical equation** HCO + H -> H2O + C ~n", []),

            Cx_id = spawn (chemistry, c, [ ]  ),

            collide({CH3OHP,CH2OHP,CH4P,CH3OP,THCO,CH3P,H2P,O2P,OP,OHP,TH,CP++[Cx_id],COP}, H2O+1, CO2, S);

        {[CH3OHP|TAIL],CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,OP,[OHP|TOH],HP,CP,COP} ->


            OHP ! {output,oh},
            CH3OHP ! {input, oh},

            io: format ("~n**chemical equation** OH + CH3OH -> CH3 + H2O + O ~n", []),

            CH3x_id = spawn (chemistry, ch3, [ ]  ),
            Ox_id = spawn (chemistry, o, [ ]  ),

            collide({TAIL,CH2OHP,CH4P,CH3OP,HCOP,CH3P++[CH3x_id],H2P,O2P,OP++[Ox_id],TOH,HP,CP,COP}, H2O+1, CO2, S);

        {[CH3OHP|TAIL],CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,OP,OHP,[HP|TH],CP,COP} ->

            HP ! {output,h},
            CH3OHP ! {input,h},

            io: format ("~n**chemical equation** H + CH3OH -> CH2OH + H2 ~n", []),

            CH2OHx_id = spawn (chemistry, ch2oh, [ ]  ),
            H2x_id = spawn (chemistry, h2, [ ]  ),

            collide({TAIL,CH2OHP++[CH2OHx_id],CH4P,CH3OP,HCOP,CH3P,H2P++[H2x_id],O2P,OP,OHP,TH,CP,COP}, H2O, CO2, S);

        {CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,[CH3P|TCH3],H2P,O2P,OP,OHP,[HP|THP],CP,COP} ->

            CH3P ! {input,h},
            HP ! {output,h},

            io: format ("~n**chemical equation** CH3 + H -> CH4 ~n", []),

            CH4x_id = spawn (chemistry, ch4, [ ]  ),

            collide({CH3OHP,CH2OHP,CH4P++[CH4x_id], CH3OP, HCOP,TCH3,H2P,O2P,OP,OHP ,THP,CP,COP}, H2O, CO2, S );

        {CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,[CH3P|TCH3],H2P,[O2P|TO2],OP,OHP,HP,CP,COP} ->

            CH3P ! {input,o},
            O2P ! {output,o},

            io: format ("~n**chemical equation** CH3 + O2 -> CH3O + O ~n", []),

            CH3Ox_id = spawn (chemistry, ch3o, [ ]  ),
            Ox_id = spawn (chemistry, o, [ ]  ),

            collide({CH3OHP,CH2OHP,CH4P,CH3OP++[CH3Ox_id],HCOP,TCH3,H2P,TO2,OP++[Ox_id],OHP ,HP,CP,COP}, H2O, CO2, S );

        {CH3OHP,CH2OHP,[CH4P|TCH4], CH3OP, HCOP,CH3P,H2P,O2P,[OP|TO],OHP,HP,CP,COP} ->

            CH4P ! {input,o},
            OP ! {output,o},

            io: format ("~n**chemical equation** CH4 + O -> CH3O + H ~n", []),

            CH3Ox_id = spawn (chemistry, ch3o, [ ]  ),
            Hx_id = spawn (chemistry, h, [ ]  ),

            collide({CH3OHP,CH2OHP,TCH4,CH3OP++[CH3Ox_id],HCOP,CH3P,H2P,O2P,TO,OHP ,HP++[Hx_id],CP,COP}, H2O, CO2, S );

        {CH3OHP,CH2OHP, CH4P,[CH3OP|TCH3O],HCOP,CH3P,[H2P|TH2],O2P,OP,OHP,HP,CP,COP} ->

            CH3OP ! {input,h},
            H2P ! {output,h},

            io: format ("~n**chemical equation** CH3O + H2 -> CH4 + O + H ~n", []),

            CH4x_id = spawn (chemistry, ch4, [ ]  ),
            Ox_id = spawn (chemistry, o, [ ]  ),
            Hx_id = spawn (chemistry, h, [ ]  ),

            collide({CH3OHP,CH2OHP,CH4P++[CH4x_id],TCH3O,HCOP,CH3P,TH2,O2P,OP++[Ox_id],OHP ,HP++[Hx_id],CP,COP}, H2O, CO2, S );

        {CH3OHP,CH2OHP,[CH4P|TCH4],CH3OP,HCOP,CH3P,H2P,O2P,OP,[OHP|TOH],HP,CP,COP} ->

            CH4P ! {input,oh},
            OHP ! {output,oh},

            io: format ("~n**chemical equation** CH4 + OH -> H2O + CH3 ~n", []),

            CH3x_id = spawn (chemistry, ch3, [ ]  ),

            collide({CH3OHP,CH2OHP,TCH4,CH3OP,HCOP,CH3P++[CH3x_id],H2P,O2P,OP,TOH,HP,CP,COP}, H2O+1, CO2, S );

        {CH3OHP,[CH2OHP|TCH2OH],CH4P,CH3OP,HCOP,CH3P,[H2P|TH2],O2P,OP,OHP,HP,CP,COP} ->

            CH2OHP ! {input,h},
            H2P ! {output,h},

            io: format ("~n**chemical equation** CH2OH + H2 -> CH4 + H + O ~n", []),

            CH4x_id = spawn (chemistry, ch4, [ ]  ),
            Hx_id = spawn (chemistry, h, [ ]  ),
            Ox_id = spawn (chemistry, o, [ ]  ),

            collide({CH3OHP,TCH2OH,CH4P++[CH4x_id],CH3OP,HCOP,CH3P,TH2,O2P,OP++[Ox_id],OHP,HP++[Hx_id],CP,COP}, H2O, CO2, S );

        {CH3OHP,CH2OHP,CH4P,CH3OP,HCOP,CH3P,H2P,[O2P|TO2],OP,OHP,HP,[CP|TCP],COP} ->

            CP ! {output,c},
            O2P ! {input,c},

            io: format ("~n**chemical equation** C + O2 -> CO2 ~n", []),

            collide({CH3OHP,CH2OHP,CH4P,CH3OP,HCOP,CH3P,H2P,TO2,OP,OHP,HP,TCP,COP}, H2O, CO2+1, S );

        {CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,[OP|TO],OHP,HP,CP,[COP|TCO]} ->

            COP ! {input,o},
            OP ! {output,o},

            io: format ("~n**chemical equation** CO + O -> CO2 ~n", []),

            collide({CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,TO,OHP,HP,CP,TCO}, H2O, CO2 +1,S );

        {CH3OHP,CH2OHP, CH4P,[CH3OP|TCH3O], HCOP,CH3P,H2P,O2P,OP,[OHP|TOH],HP,CP,COP} ->

            CH3OP ! {input,oh},
            OHP ! {output,oh},

            io: format ("~n**chemical equation** CH3O + OH -> HCO + H + H2O ~n", []),

            HCOx_id = spawn (chemistry, hco, []  ),
            Hx_id = spawn (chemistry, h, [ ]  ),

            collide({CH3OHP,CH2OHP, CH4P, TCH3O, HCOP++[HCOx_id],CH3P,H2P,O2P,OP,TOH,HP++[Hx_id],CP,COP}, H2O + 1, CO2, S );

        {CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,[OP|TO],OHP,[HP|TH],CP,COP} ->

            OP ! {input,h},
            HP ! {output,h},

            io: format ("~n**chemical equation** H + O -> OH ~n", []),

            OHx_id = spawn (chemistry, oh, []  ),

            collide({CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,TO,OHP++[OHx_id],TH,CP,COP}, H2O, CO2, S  );

        {CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,[H2P|THP],O2P,[OP|TO],OHP,HP,CP,COP} ->

            H2P ! {input,o},
            OP ! {output,o},

            io: format ("~n**chemical equation** H2 + O -> H2O ~n", []),

            collide({CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,THP,O2P,TO,OHP,HP,CP,COP}, H2O + 1, CO2, S  );

        {CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,[H2P|THP],O2P,OP,[OHP|TOH],HP,CP,COP} ->

            H2P ! {input, oh},
            OHP ! {output, oh},

            io: format ("~n**chemical equation** H2 + OH -> H2O + H ~n", []),

            Hx_id = spawn (chemistry, h, []  ),

            collide({CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,THP,O2P,OP,TOH,HP++[Hx_id],CP,COP}, H2O + 1, CO2, S  );

        {CH3OHP,CH2OHP, CH4P, CH3OP,[HCOP|THCO],CH3P,H2P,[O2P|TO2],OP,OHP,HP,CP,COP} ->

            O2P ! {input, c},
            HCOP ! {output, c},

            io: format ("~n**chemical equation** HCO + O2 -> CO2 + OH ~n", []),

            OHx_id = spawn (chemistry, oh, [ ]  ),

            collide({CH3OHP,CH2OHP,CH4P,CH3OP,THCO,CH3P,H2P,TO2,OP,OHP++[OHx_id],HP,CP,COP}, H2O, CO2+1, S);

        {CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,[O2P|TO2],OP,OHP,[HP|TH],CP,COP} ->

            O2P ! {input,h},
            HP ! {output,h},

            io: format ("~n**chemical equation** O2 + H -> OH + H ~n", []),

            OHx_id = spawn (chemistry, oh, []  ),
            Ox_id = spawn (chemistry, o, []  ),

            collide({CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,TO2,OP++[Ox_id],OHP++[OHx_id],TH,CP,COP}, H2O, CO2, S  );
        
        {CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,OP,[OHP|TOH],[HP|TH],CP,COP} ->

            HP ! {output,h},
            OHP ! {input,h},

            io: format ("~n**chemical equation** OH + H -> H2O ~n", []),

            collide({CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,OP,TOH ,TH,CP,COP}, H2O +1, CO2, S);
        {[CH3OHP|TAIL],CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,[O2P|TO2],OP,OHP,HP,CP,COP} ->

            O2P ! {output,o},
            CH3OHP ! {input,o},

            io: format ("~n**chemical equation** CH3OH + O2 -> O + HCO + OH + 2H ~n", []),

            Hx_id1 = spawn (chemistry, h, [ ]  ),
            Hx_id2 = spawn (chemistry, h, [ ]  ),
            OHx_id = spawn (chemistry, oh, [ ]  ),
            Ox_id = spawn (chemistry, o, [ ]  ),
            HCOx_id = spawn (chemistry, hco, [ ]  ),

            collide({TAIL,CH2OHP,CH4P,CH3OP,HCOP++[HCOx_id],CH3P,H2P,TO2,OP++[Ox_id],OHP++[OHx_id],HP++[Hx_id1,Hx_id2],CP,COP},H2O,CO2, S);
        
        {CH3OHP,CH2OHP, CH4P, CH3OP, HCOP,CH3P,H2P,O2P,OP,OHP,HP,CP,COP} ->
            io: format ("~n ********************** FINAL MESSAGE *********************** ~n", []),
            io: format ("~n ***** ~p ***** ~n", [S]),

            L = [{length(CH3OHP), "CH3OHP"}, {length(CH2OHP), "CH2OHP"}, {length(CH4P), "CH4"}, {length(CH3OP), "CH3O"}, {length(HCOP), "HCO"}, 
            {length(CH3P), "CH3"}, {length(H2P), "H2"}, {length(O2P), "O2"}, {length(OP), "O"}, {length(OHP), "OH"}, {length(HP), "H"}, 
            {length(CP), "C"}, {length(COP), "CO"}, {H2O, "H2O"}, {CO2, "CO2"}],

            io: format ("~n Reaction finished with ", []),
            printfinal(L)
        end.

% Function to print out the final state.
printfinal(L) ->
        case L of
            [] ->
                io: format (" molecules ~n~n", []);           
            [{0,_}|Tail] ->
                printfinal(Tail);
            [{N,S}|Tail] ->
                io: format (" ~p ~p ", [N,S]),
                printfinal(Tail)
            end.
