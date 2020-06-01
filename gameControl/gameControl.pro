% Copyright

implement gameControl inherits userControlSupport
    open core, vpiDomains, cellControl

clauses
    new(Parent) :-
        new(),
        setContainer(Parent).

clauses
    new() :-
        userControlSupport::new(),
        generatedInitialize(),
        createControls().

constants
    human : positive = 1.

facts
    m : integer := 8.
    n : integer := 8.
    gameFrm : gameForm := erroneous.
    white : picture := erroneous.
    white_mask : picture := erroneous.
    white_king : picture := erroneous.
    white_king_mask : picture := erroneous.
    black : picture := erroneous.
    black_mask : picture := erroneous.
    black_king : picture := erroneous.
    black_king_mask : picture := erroneous.
    waiting : picture := erroneous.
    waitingSize : rct := rct(0, 0, 1, 1).
    waiting_mask : picture := erroneous.
    whiteSize : rct := rct(0, 0, 1, 1).
    blackSize : rct := rct(0, 0, 1, 1).
    currentPosition : position := pos([], []).
    fromSq : optional{sq} := core::none().
    toSq : optional{sq} := core::none().
    isGameOver : boolean := false.
    level : positive := 2.
    startMove : boolean := false.
    wait : boolean := false.

domains
    checker = ch(sq, positive IsDamka, positive Color).
    position = pos(checker* Comp, checker* Human).
    move =
        move(checker From, checker To, tuple{checker Eaten, checker Move}* Food);
        nil.

predicates
    initPosition : ().
    getCell : (integer, integer) -> cellControl determ.

clauses
    getCell(I, J) = Cell :-
        cell(I, J, Cell),
        !.
    initPosition() :-
        foreach I = std::fromTo(0, 2) and J = std::fromTo(0, 7) and (I + J) mod 2 = 1 and Cell = getCell(I, J) do
            Cell:setCellState(cellControl::white)
        end foreach,
        foreach I = std::fromTo(5, 7) and J = std::fromTo(0, 7) and (I + J) mod 2 = 1 and Cell = getCell(I, J) do
            Cell:setCellState(cellControl::black)
        end foreach,
        WhiteL =
            [ ch(sq(I1, J1), 0, 0) ||
                cell(I1, J1, Cell),
                Cell:state = cellControl::white
            ],
        BlackL =
            [ ch(sq(I2, J2), 0, 1) ||
                cell(I2, J2, Cell2),
                Cell2:state = cellControl::black
            ],
        if gameFrm:compColor = color_white then
            currentPosition := pos(WhiteL, BlackL)
        else
            currentPosition := pos(BlackL, WhiteL)
        end if.

facts
    cell : (integer I, integer J, cellControl).

predicates
    createControls : ().
    setControls : ().

clauses
    createControls() :-
        foreach I = std::fromTo(0, m - 1) and J = std::fromTo(0, n - 1) do
            Cell = cellControl::new(This),
            Cell:i := I,
            Cell:j := J,
            if (I + J) mod 2 = 0 then
                Cell:even := true
            else
                Cell:even := false
            end if,
            Cell:gameCtl := This,
            assert(cell(I, J, Cell))
        end foreach.

    setControls() :-
        getSize(W, H),
        Wcell = W div n,
        Hcell = H div m,
        foreach cell(I, J, Cell) do
            Cell:setSize(Wcell, Hcell),
            Cell:setPosition(J * Wcell, I * Hcell)
        end foreach.

predicates
    loadPict : ().
clauses
    loadPict() :-
        Filename1 = "black.bmp",
        try
            Pict1 = vpi::pictLoad(Filename1)
        catch Error1 do
            stdio::writef("Error %. Unabel to load a picture from %\n", Error1, Filename1),
            fail
        end try,
        black := Pict1,
        vpi::pictGetSize(Pict1, W1, H1, _),
        blackSize := rct(0, 0, W1, H1),
        %%%%%%%
        Filename2 = "black_mask.bmp",
        try
            Pict2 = vpi::pictLoad(Filename2)
        catch Error2 do
            stdio::writef("Error %. Unabel to load a picture from %\n", Error2, Filename2),
            fail
        end try,
        black_mask := Pict2,
        %%%%%%%%
        Filename3 = "white.bmp",
        try
            Pict3 = vpi::pictLoad(Filename3)
        catch Error3 do
            stdio::writef("Error %. Unabel to load a picture from %\n", Error3, Filename3),
            fail
        end try,
        white := Pict3,
        vpi::pictGetSize(Pict3, W3, H3, _),
        whiteSize := rct(0, 0, W3, H3),
        %%%%%%%%
        Filename4 = "white_mask.bmp",
        try
            Pict4 = vpi::pictLoad(Filename4)
        catch Error4 do
            stdio::writef("Error %. Unabel to load a picture from %\n", Error4, Filename4),
            fail
        end try,
        white_mask := Pict4,
        %%%%%%%%
        Filename5 = "black_king.bmp",
        try
            Pict5 = vpi::pictLoad(Filename5)
        catch Error5 do
            stdio::writef("Error %. Unabel to load a picture from %\n", Error5, Filename5),
            fail
        end try,
        black_king := Pict5,
        vpi::pictGetSize(Pict5, W5, H5, _),
        blackSize := rct(0, 0, W5, H5),
        %%%%%%%%
        Filename6 = "black_king_mask.bmp",
        try
            Pict6 = vpi::pictLoad(Filename6)
        catch Error6 do
            stdio::writef("Error %. Unabel to load a picture from %\n", Error6, Filename6),
            fail
        end try,
        black_king_mask := Pict6,
        %%%%%%%%
        Filename7 = "white_king.bmp",
        try
            Pict7 = vpi::pictLoad(Filename7)
        catch Error7 do
            stdio::writef("Error %. Unabel to load a picture from %\n", Error7, Filename7),
            fail
        end try,
        white_king := Pict7,
        vpi::pictGetSize(Pict7, W7, H7, _),
        whiteSize := rct(0, 0, W7, H7),
        %%%%%
        Filename9 = "waiting.bmp",
        try
            Pict9 = vpi::pictLoad(Filename9)
        catch Error9 do
            stdio::writef("Error %. Unabel to load a picture from %\n", Error9, Filename9),
            fail
        end try,
        waiting := Pict9,
        vpi::pictGetSize(Pict9, W9, H9, _),
        waitingSize := rct(0, 0, W9, H9),
        %%%%%
        Filename10 = "waiting_mask.bmp",
        try
            Pict10 = vpi::pictLoad(Filename10)
        catch Error10 do
            stdio::writef("Error %. Unabel to load a picture from %\n", Error10, Filename10),
            fail
        end try,
        waiting_mask := Pict10,
        %%%%%%%%
        Filename8 = "white_king_mask.bmp",
        try
            Pict8 = vpi::pictLoad(Filename8)
        catch Error8 do
            stdio::writef("Error %. Unabel to load a picture from %\n", Error8, Filename8),
            fail
        end try,
        white_king_mask := Pict8,
        !.

    loadPict().

    startHumanMove(Cell) :-
        Sq = sq(Cell:i, Cell:j),
        toSq = core::none(),
        !,
        fromSq := some(Sq).

    startHumanMove(_).

    finishHumanMove(Cell) :-
        fromSq = some(Sq1),
        currentPosition = pos(Comp, Human),
        HumanCh in Human,
        HumanCh = ch(Sq1, _T, Col),
        Sq2 = sq(Cell:i, Cell:j),
        CompCh in Comp,
        CompCh = ch(Sq, _, _),
        between(Sq1, Sq2, Sq),
        Comp1 = list::remove(Comp, CompCh),
        Human0 = list::remove(Human, HumanCh),
        if isBorder(Sq2) then
            T1 = 1
        else
            T1 = 0
        end if,
        To = ch(Sq2, T1, Col),
        Human1 = [To | Human0],
        currentPosition := pos(Comp1, Human1),
        Sq1 = sq(I1, J1),
        Cell1 = getCell(I1, J1),
        Cell1:setCellState(empty),
        Sq = sq(I, J),
        Cell3 = getCell(I, J),
        Cell3:setCellState(empty),
        Sq2 = sq(I2, J2),
        Cell2 = getCell(I2, J2),
        !,
        State = if isBorder(Sq2) then cellControl::white_d else cellControl::white end if,
        Cell2:setCellState(State),
        fromSq := core::none().

    finishHumanMove(Cell) :-
        fromSq = some(Sq1),
        currentPosition = pos(Comp, Human),
        HumanCh in Human,
        HumanCh = ch(Sq1, _T, Col),
        Sq2 = sq(Cell:i, Cell:j),
        Human0 = list::remove(Human, HumanCh),
        if isBorder(Sq2) then
            T1 = 1
        else
            T1 = 0
        end if,
        To = ch(Sq2, T1, Col),
        Human1 = [To | Human0],
        currentPosition := pos(Comp, Human1),
        Sq1 = sq(I1, J1),
        Cell1 = getCell(I1, J1),
        Cell1:setCellState(empty),
        Sq2 = sq(I2, J2),
        Cell2 = getCell(I2, J2),
        !,
        State = if isBorder(Sq2) then cellControl::white_d else cellControl::white end if,
        Cell2:setCellState(State),
        fromSq := core::none(),
        gameFrm:setText("Ход компьютера").

    finishHumanMove(_).

predicates
    move_nd : (position) -> move nondeterm.
    getFood : (checker From, checker* Comp, checker* Human, checker To [out], tuple{checker Eaten, checker Move}* Food)
        -> tuple{checker Eaten, checker Move}* AllEaten nondeterm.
    getFood1 : (checker From, checker* Comp, checker* Human, checker To [out], tuple{checker Eaten, checker Move}* Food)
        -> tuple{checker Eaten, checker Move}* AllEaten nondeterm.
    canEat : (checker From, checker* Comp, checker* Human, checker Eaten [out], checker To [out]) nondeterm.
    canEat1 : (checker From, checker* Comp, checker* Human, checker Eaten [out], checker To [out]) nondeterm.
    diagInv : (sq, sq) -> integer determ.
    dist : (sq, sq) -> integer determ.
    %расстояние
    between : (sq From, sq To, sq Middle) determ.
    diagSq : (sq) -> sq nondeterm.
    %произвольная клетка на той же диагонали
    checkIfNear : (sq, sq) -> integer determ.
    checkIfNear2 : (sq, sq) -> integer determ.
    isBorder : (sq) determ.
    gameOver : () determ.
    humanMove : ().

clauses
    gameOver() :-
        currentPosition = pos([], _),
        !.

    gameOver() :-
        currentPosition = pos(_, []),
        !.

    gameOver() :-
        not(_ = move_nd(currentPosition)),
        !.

    gameOver() :-
        not(_ = move_nd(invert(currentPosition))),
        !.

    humanMove() :-
        gameOver(),
        !,
        vpiCommonDialogs::note("Компьютер выиграл!").

    humanMove() :-
        gameFrm:setText("Ваш ход").

    compMove() :-
        gameOver(),
        !,
        isGameOver := true,
        vpiCommonDialogs::note("Человек выиграл!").
    compMove() :-
        move(From, To, L) = compMove(currentPosition),
        currentPosition := insert(move(From, To, L), currentPosition),
        From = ch(Sq1, _, _),
        Sq1 = sq(I1, J1),
        Cell1 = getCell(I1, J1),
        Cell1:setCellState(empty),
        To = ch(Sq2, Type, _),
        Sq2 = sq(I2, J2),
        Cell2 = getCell(I2, J2),
        !,
        State = if Type = 1 then cellControl::black_d else cellControl::black end if,
        Cell2:setCellState(State),
        startShowMove(move(From, To, L)).
        %!
    compMove() :-
        vpiCommonDialogs::note("Ошибка в ходе").

facts
    step : positive := human.
    timer : timerHandle := erroneous.
    currentDisks : tuple{checker Eaten, checker Move}* := [].

predicates
    startShowMove : (move).
    showMove : (positive).

clauses
    startShowMove(move(_From, _To, L)) :-
        !,
        wait := true,
        currentDisks := L,
        step := 2,
        timer := timerSet(700).
    startShowMove(_).
    showMove(2) :-
        !,
        foreach tuple(ch(Sq, _, _), _) in currentDisks and Sq = sq(I, J) and Cell = getCell(I, J) do
            Cell:setCellState(cellControl::waiting)
        end foreach.

    showMove(_) :-
        foreach tuple(ch(Sq, _, _), _) in currentDisks and Sq = sq(I, J) and Cell = getCell(I, J) do
            Cell:setCellState(empty)
        end foreach,
        wait := false.

predicates
    compMove : (position) -> move.
    value : (position) -> integer.
    getMoves : (position) -> tuple{integer Value, position, move}*.
    invert : (position) -> position.
    insert : (move, position) -> position determ.

clauses
    getMoves(Position) =
        [ tuple(Value, NewPosition, Move) ||
            Move = move_nd(Position),
            NewPosition = insert(Move, Position),
            Value = value(Position)
        ].

    insert(move(From, To, SqL), pos(L1, L2)) = pos(NewL1, NewL2) :-
        L1_1 = list::remove(L1, From),
        NewL1 = [To | L1_1],
        EL = list::map(SqL, { (tuple(X, _)) = X }),
        NewL2 = list::difference(L2, EL).

    invert(pos(L1, L2)) = pos(L2, L1).

    value(pos(L1, L2)) = list::length(L1) - list::length(L2).

    compMove(Position) = Move :-
        level > 1,
        V = n * m,
        alphaBeta(level, Position, -V, V, Move, _),
        !.

    compMove(Position) = Move :-
        level := 1,
        Moves = getMoves(Position),
        Moves <> [],
        !,
        tuple(_, _, Move) = list::maximum(Moves).

    compMove(Position) = Move :-
        Moves = getMoves(Position),
        Moves <> [],
        !,
        N = list::length(Moves),
        K = math::random(N),
        tuple(_, _, Move) = list::nth(K, Moves).

    compMove(_Position) = _ :-
        exception::raise_error().

predicates
    alphaBeta : (integer, position, integer, integer, move [out], integer [out]) nondeterm.
    bestMove : (tuple{integer, position, move}*, integer, integer, integer, move, move [out], integer [out]) nondeterm.
    tryBetaPruning : (move, integer, integer, integer, integer, tuple{integer, position, move}*, move, move [out], integer [out]) nondeterm.
    tryIncreaseAlpha : (move, move, integer, integer, move [out], integer [out]).

clauses
    alphaBeta(D, Position, Alpha, Beta, Move, Value) :-
        D > 0,
        MoveList = getMoves(Position),
        Moves = list::sort(MoveList, descending()),
        bestMove(Moves, D - 1, Alpha, Beta, nil, Move, Value).

    alphaBeta(0, Position, _, _, nil, value(Position)).

    bestMove([tuple(_, Position, Move) | Moves], D, Alpha, Beta, CurrMove, BestMove, BestValue) :-
        alphaBeta(D, invert(Position), -Beta, -Alpha, _, Value),
        tryBetaPruning(Move, -Value, D, Alpha, Beta, Moves, CurrMove, BestMove, BestValue).

    bestMove([], _, Alpha, _, Move, Move, Alpha).

    tryBetaPruning(Move, Value, _, _, Beta, _, _, Move, Value) :-
        Value >= Beta,
        !.

    tryBetaPruning(Move, Value, D, Alpha, Beta, Moves, CurrMove, BestMove, BestValue) :-
        tryIncreaseAlpha(Move, CurrMove, Value, Alpha, CurrMove1, Alpha1),
        bestMove(Moves, D, Alpha1, Beta, CurrMove1, BestMove, BestValue).

    tryIncreaseAlpha(Move, _, Value, Alpha, Move, Value) :-
        Value > Alpha,
        !.

    tryIncreaseAlpha(_, CurrMove, _, Alpha, CurrMove, Alpha).

clauses
    isBorder(sq(0, _)) :-
        !.

    isBorder(sq(m - 1, _)).

    diagSq(sq(I, J)) = sq(I1, J1) :-
        S = I + J,
        I1 = std::fromTo(0, 7),
        J1 = S - I1,
        J1 >= 0,
        J1 < 8.

    diagSq(sq(I, J)) = sq(I1, J1) :-
        S = I - J,
        I1 = std::fromTo(0, 7),
        J1 = I1 - S,
        J1 >= 0,
        J1 < 8.

    between(From, To, Middle) :-
        D = diagInv(From, To),
        D = diagInv(From, Middle),
        R = dist(From, To),
        dist(From, Middle) < R,
        dist(To, Middle) < R.

    dist(sq(I1, J1), sq(I2, J2)) = math::abs(I1 - I2) + math::abs(J1 - J2) :-
        _ = diagInv(sq(I1, J1), sq(I2, J2)).

    diagInv(sq(I1, J1), sq(I2, J2)) = I1 + J1 :-
        I1 + J1 = I2 + J2,
        !.

    diagInv(sq(I1, J1), sq(I2, J2)) = I1 - J1 :-
        I1 - J1 = I2 - J2,
        !.

    checkIfNear(sq(I1, J1), sq(I2, J2)) = J2 :-
        I2 = I1 - 1,
        J2 = J1 - 1,
        !.

    checkIfNear(sq(I1, J1), sq(I2, J2)) = J2 :-
        I2 = I1 - 1,
        J2 = J1 + 1,
        !.

    checkIfNear2(sq(I1, J1), sq(I2, J2)) = J2 :-
        I2 = I1 + 1,
        J2 = J1 - 1,
        !.

    checkIfNear2(sq(I1, J1), sq(I2, J2)) = J2 :-
        I2 = I1 + 1,
        J2 = J1 + 1,
        !.

    move_nd(pos(Comp, Human)) = Move :-
        %ход, где дамка есть максимальное количество шашек

        From in Comp,
        From = ch(_Sq, 1, _Col),
        LL = [ tuple(list::length(L), L, To1) || L = getFood(From, Comp, Human, To1, []) ],
        LL <> [],
        tuple(_, L1, To) = list::maximum(LL),
        Move = move(From, To, L1).

    move_nd(pos(Comp, Human)) = Move :-
        %ест  простая шашка
        From in Comp,
        From = ch(_Sq, 0, _Col),
        LL = [ tuple(list::length(L), L, To1) || L = getFood1(From, Comp, Human, To1, []) ],
        LL <> [],
        tuple(_, L1, To) = list::maximum(LL),
        To = ch(CompSq, _, Col),
        (not(isBorder(CompSq)) and Type = 0 or isBorder(CompSq) and Type = 1),
        To2 = ch(CompSq, Type, Col),
        Move = move(From, To2, L1).

    move_nd(pos(Comp, Human)) = Move :-
        % ход дамки
        From in Comp,
        From = ch(sq(I, J), 1, Col),
        CompSq = diagSq(sq(I, J)),
        CompSq = sq(I1, J1),
        _D = diagInv(sq(I, J), sq(I1, J1)),
        %проверяем, что они на одной диагонали
        not(ch(CompSq, _, _) in Comp),
        %на новой позиции никого
        not(ch(CompSq, _, _) in Human),
        not((Ch in Human and Ch = ch(Sq, _, _) and between(sq(I, J), sq(I1, J1), Sq))),
        %между нами и новой позицией нет шашек
        not((ch(Sq1, _, _) in Comp and between(sq(I, J), sq(I1, J1), Sq1))),
        To = ch(CompSq, 1, Col),
        Move = move(From, To, []).

    move_nd(pos(Comp, Human)) = Move :-
        % ход шашки, просто вперед на один
        From in Comp,
        From = ch(sq(I, J), 0, Col),
        CompSq = diagSq(sq(I, J)),
        CompSq = sq(I1, J1),
        (Col = 1 and _J2 = checkIfNear(sq(I, J), sq(I1, J1)) or Col = 0 and _J2 = checkIfNear2(sq(I, J), sq(I1, J1))),
        %проверяем, что новая позиция рядом (по диагонали)
        not(ch(CompSq, _, _) in Comp),
        not(ch(CompSq, _, _) in Human),
        % на новой позиции нет никаких шашек
        (not(isBorder(CompSq)) and Type = 0 or isBorder(CompSq) and Type = 1),
        To = ch(CompSq, Type, Col),
        Move = move(From, To, []).

    canEat(ch(sq(I, J), 1, C), Comp, Human, Eaten, To) :-
        Ch in Human,
        Ch = ch(sq(I1, J1), _, _),
        D = diagInv(sq(I, J), sq(I1, J1)),
        %проверяем, что они на одной диагонали
        CompSq = diagSq(sq(I, J)),
        %находим новую позицию на этой диагонали
        D = diagInv(sq(I, J), CompSq),
        %вторая проверка, что новая позиция на диагонали с жертвой
        not(ch(CompSq, _, _) in Comp),
        %на новой позиции никого
        not(ch(CompSq, _, _) in Human),
        between(sq(I, J), CompSq, sq(I1, J1)),
        % жертва стоит между нами и нашей новой позицией
        not((ch(Sq, _, _) in Human and between(sq(I, J), sq(I1, J1), Sq))),
        %между нами и жертвой нет шашек
        not((ch(Sq1, _, _) in Comp and between(sq(I, J), sq(I1, J1), Sq1))),
        not((ch(Sq2, _, _) in Human and between(sq(I1, J1), CompSq, Sq2))),
        not((ch(Sq3, _, _) in Comp and between(sq(I1, J1), CompSq, Sq3))),
        Eaten = Ch,
        To = ch(CompSq, 1, C).

    getFood(From, Comp, Human, To, L) = getFood(From1, Comp1, Human1, To, [tuple(Eaten, From1) | L]) :-
        canEat(From, Comp, Human, Eaten, From1),
        Human1 = list::remove(Human, Eaten),
        Comp1 = [From1 | list::remove(Comp, From)].

    getFood(_, _, _, To, L) = list::reverse(L) :-
        L = [tuple(_, To) | _].

    canEat1(ch(sq(I, J), 0, C), Comp, Human, Eaten, To) :-
        ch(sq(I, J), 0, C) in Comp,
        Ch in Human,
        Ch = ch(sq(I1, J1), _, _),
        D = diagInv(sq(I, J), sq(I1, J1)),

        %проверяем, что они на одной диагонали и соседние
        CompSq = diagSq(sq(I, J)),
        (_ = checkIfNear(sq(I, J), sq(I1, J1)) and _ = checkIfNear(sq(I1, J1), CompSq)
            or _ = checkIfNear2(sq(I, J), sq(I1, J1)) and _ = checkIfNear2(sq(I1, J1), CompSq)),
        %находим новую позицию на этой диагонали
        D = diagInv(sq(I, J), CompSq),
        % dist(CompSq, sq(I1, J1)) = 2,
        %вторая проверка, что новая позиция на диагонали с жертвой
        not(ch(CompSq, _, _) in Comp),
        %на новой позиции никого
        not(ch(CompSq, _, _) in Human),
        between(sq(I, J), CompSq, sq(I1, J1)),
        % жертва стоит между нами и нашей новой позицией

        Eaten = Ch,
        To = ch(CompSq, 0, C).

    getFood1(From, Comp, Human, To, L) = getFood1(From1, Comp1, Human1, To, [tuple(Eaten, From1) | L]) :-
        canEat1(From, Comp, Human, Eaten, From1),
        Human1 = list::remove(Human, Eaten),
        Comp1 = [From1 | list::remove(Comp, From)].

    getFood1(_, _, _, To, L) = list::reverse(L) :-
        L = [tuple(_, To) | _].

predicates
    onShow : window::showListener.
clauses
    onShow(_Source, _Data) :-
        setControls(),
        loadPict(),
        initPosition().

predicates
    onSize : window::sizeListener.
clauses
    onSize(_Source) :-
        setControls().

predicates
    onTimer : window::timerListener.
clauses
    onTimer(_Source, _Timer) :-
        step > 0,
        !,
        showMove(step),
        step := step - 1.

    onTimer(_Source, _Timer) :-
        timerKill(timer),
        timer := erroneous,
        humanMove().

% This code is maintained automatically, do not update it manually. 11:33:02-30.4.2018
predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("gameControl"),
        This:setSize(111, 70),
        addShowListener(onShow),
        addSizeListener(onSize),
        addTimerListener(onTimer).
    % end of automatic code

end implement gameControl
