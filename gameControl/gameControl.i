% Copyright

interface gameControl supports control
    open core, vpiDomains

domains
    sq = sq(integer, integer).

properties
    gameFrm : gameForm.
    white : picture.
    white_mask : picture.
    white_king : picture.
    white_king_mask : picture.
    black : picture.
    black_mask : picture.
    black_king : picture.
    black_king_mask : picture.
    whiteSize : rct.
    blackSize : rct.
    fromSq : optional{sq}.
    toSq : optional{sq}.
    isGameOver : boolean.
    waiting : picture.
    waiting_mask : picture.
    waitingSize : rct.
    startMove : boolean.
    wait : boolean.

predicates
    startHumanMove : (cellControl).
    finishHumanMove : (cellControl).
    compMove : ().

end interface gameControl
