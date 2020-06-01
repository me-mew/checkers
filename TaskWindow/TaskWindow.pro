% Copyright

implement taskWindow inherits applicationWindow
    open core, vpiDomains

constants
    mdiProperty : boolean = true.

clauses
    new() :-
        applicationWindow::new(),
        generatedInitialize().

predicates
    onShow : window::showListener.
clauses
    onShow(_, _CreationData) :-
        _MessageForm = messageForm::display(This).

class predicates
    onDestroy : window::destroyListener.
clauses
    onDestroy(_).

class predicates
    onHelpAbout : window::menuItemListener.
clauses
    onHelpAbout(TaskWin, _MenuTag) :-
        _AboutDialog = aboutDialog::display(TaskWin).

predicates
    onFileExit : window::menuItemListener.
clauses
    onFileExit(_, _MenuTag) :-
        close().

predicates
    onSizeChanged : window::sizeListener.
clauses
    onSizeChanged(_) :-
        vpiToolbar::resize(getVPIWindow()).

predicates
    onFileNew : window::menuItemListener.
clauses
    onFileNew(_Source, _MenuTag) :-
        _ = settingsDialog::display(This).
        % This code is maintained automatically, do not update it manually. 13:38:37-23.4.2018

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("checkers"),
        setDecoration(titlebar([closeButton, maximizeButton, minimizeButton])),
        setBorder(sizeBorder()),
        setState([wsf_ClipSiblings, wsf_Maximized]),
        whenCreated({  :- projectToolbar::create(getVpiWindow()) }),
        addSizeListener({  :- vpiToolbar::resize(getVpiWindow()) }),
        setMdiProperty(mdiProperty),
        menuSet(resMenu(resourceIdentifiers::id_TaskMenu)),
        addShowListener(onShow),
        addSizeListener(onSizeChanged),
        addDestroyListener(onDestroy),
        addMenuItemListener(resourceIdentifiers::id_help_about, onHelpAbout),
        addMenuItemListener(resourceIdentifiers::id_file_exit, onFileExit),
        addMenuItemListener(resourceIdentifiers::id_file_new, onFileNew).
    % end of automatic code

end implement taskWindow
