% Copyright

implement gameForm inherits formWindow
    open core, vpiDomains

clauses
    display(Parent) = Form :-
        Form = new(Parent),
        Form:show().

clauses
    new(Parent) :-
        formWindow::new(Parent),
        generatedInitialize(),
        gameControl_ctl:gameFrm := This.

facts
    name : string := "Noname".
    humanColor : color := color_Black.

    compColor : color := color_White.

predicates
    onShow : window::showListener.
clauses
    onShow(_Source, _Data) :-
        setText("Ваш ход").

predicates
    onStartClick : button::clickResponder.
clauses
    onStartClick(_Source) = button::defaultAction :-
        gameControl_ctl:compMove().

% This code is maintained automatically, do not update it manually. 12:43:44-30.5.2018
facts
    ok_ctl : button.
    help_ctl : button.
    gameControl_ctl : gamecontrol.
    start_ctl : button.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("gameForm"),
        setRect(rct(50, 40, 310, 292)),
        setDecoration(titlebar([closeButton, maximizeButton, minimizeButton])),
        setBorder(sizeBorder()),
        setState([wsf_ClipSiblings, wsf_ClipChildren]),
        menuSet(noMenu),
        addShowListener(onShow),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(100, 234),
        ok_ctl:setSize(56, 16),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        help_ctl := button::new(This),
        help_ctl:setText("&Help"),
        help_ctl:setPosition(168, 234),
        help_ctl:setSize(56, 16),
        help_ctl:defaultHeight := false,
        help_ctl:setAnchors([control::right, control::bottom]),
        gameControl_ctl := gamecontrol::new(This),
        gameControl_ctl:setPosition(8, 6),
        gameControl_ctl:setSize(224, 224),
        gameControl_ctl:setAnchors([control::left, control::top, control::right, control::bottom]),
        start_ctl := button::new(This),
        start_ctl:setText("Ход компьютера"),
        start_ctl:setPosition(8, 234),
        start_ctl:setWidth(68),
        start_ctl:defaultHeight := true,
        start_ctl:setAnchors([control::right, control::bottom]),
        start_ctl:setClickResponder(onStartClick).
    % end of automatic code

end implement gameForm
