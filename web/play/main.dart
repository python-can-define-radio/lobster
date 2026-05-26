/// LOBSTER Game.
library;


import 'dart:async';
import 'dart:js_interop';
import 'dart:math' hide e;
import 'package:meta/meta.dart';
import 'package:web/web.dart' hide window;
import '../dartlib/coordinates.dart';
import '../dartlib/generic.dart';
import '../dartlib/htmlhelp.dart';
import '../dartlib/mapcontrol.dart';
import '../dartlib/lobs.dart';



class Consts {
    static final msgRtn = "Return to mission selection";
}


class PlayerPos {
    final Stream<DirXY> dirxyStm;
    final Stream<Pos> posStm;
    final Observable<Pos> posObs;
    final Observable<bool> runningObs;

    PlayerPos(this.dirxyStm, this.posStm, this.posObs, this.runningObs);
    
    @factory
    static PlayerPos create(Pos initPos, KbStm keydown, KbStm keyup, DuStm tdelta) {
        final pressedStm = _makePressed(keydown, keyup);
        final dirxyStm = _makeDirXY(pressedStm);
        final runningObs = _makeRunning(pressedStm);
        final posStm = _makePos(initPos, tdelta, dirxyStm, runningObs);
        final posObs = Observable(initPos, posStm);
        return PlayerPos(dirxyStm, posStm, posObs, runningObs);
    }

    static Stream<ImmuSet<String>> _makePressed(KbStm keydown, KbStm keyup) {
        final pressed = <String>{};
        final sc = StreamController<ImmuSet<String>>.broadcast();
        keydown.listen((e) {
            if (!e.repeat) {
                pressed.add(e.code);
                sc.add(ImmuSet(pressed));
            }
        });
        keyup.listen((e) {
            pressed.remove(e.code);
            sc.add(ImmuSet(pressed));
        });
        return sc.stream.asBroadcastStream();
    }

    static Observable<bool> _makeRunning(Stream<ImmuSet<String>> pressedStm) {
        final stm = pressedStm.map((pressed) =>
            pressed.contains("ShiftLeft") || pressed.contains("ShiftRight")
        );
        return Observable(false, stm);
    }

    static Stream<DirXY> _makeDirXY(Stream<ImmuSet<String>> pressedStm) {
        return pressedStm
            .map((pressed) => DirXY.fromPressed(pressed))
            .asBroadcastStream();
    }

    static Stream<Pos> _makePos(Pos initPos, DuStm tdelta, Stream<DirXY> dirxyStm, Observable<bool> runningObs) {
        final dirxyObs = Observable(DirXY(0, 0), dirxyStm);
        return tdelta.scan(
            initPos, (prev, dt) => _computeNewPos(prev, dt, dirxyObs.latestVal, runningObs.latestVal)
        ).asBroadcastStream();
    }
    
    static Pos _computeNewPos(Pos prev, Duration dt, DirXY dirxy, bool running) {
        const baseSpeed = 2.0 * 0.001;
        final speed = running ? baseSpeed * 2 : baseSpeed;
        final dist = speed * dt.inMilliseconds;
        final (xdiff, ydiff) = (dist * dirxy.horiz, dist * dirxy.vert);
        return (prev + Pos(GC(xdiff), GC(ydiff)));
    }
}

class PlayerHUD with Displayable {
    @override
    final Box<HTMLElement> disp;
    PlayerHUD(this.disp);
    static PlayerHUD create(Stream<Pos> posStm) {
        final posEl = HTML.div()..id = "player-pos";
        posStm.listen((pos) =>
            posEl.innerText =
                "grid: 55P DE "
                "${pos.x.asfivedig} "
                "${pos.y.asfivedig}"
        );
        return PlayerHUD(Box(posEl));
    }
}

class Avatar implements Drawable {
    final HTMLImageElement _avatarSheet;
    final Observable<DirXY> _dirxyObs;
    final int _horizFrames = 4;
    final int _vertFrames = 4;
    final Observable<int> _cycle;
    final Observable<Pos> _p1posObs;

    Avatar(this._avatarSheet, this._dirxyObs, this._cycle, this._p1posObs);

    @Eff("http-req")
    @factory
    static Future<Avatar> create(Stream<DirXY> dirxyStm, Observable<bool> runningObs, Observable<Pos> p1posObs) async {
        final img = await imageload("../assets/avatar_sheet2.png");
        /// Keep only non-zero directions when determining facing
        /// so that the avatar persists facing the most recent direction
        /// when player stops moving
        final nonz = dirxyStm.where((dirxy) => !dirxy.isZero);
        final dirxyObs = Observable(DirXY(0, -1), nonz);
        final cycle = Observable(0, makeAnimCycler(dirxyStm, runningObs));
        return Avatar(img, dirxyObs, cycle, p1posObs);
    }

    /// Given DirXY, compute the row that corresponds to the facing direction
    static int _dxyToSlice(DirXY dirxy) {
        return switch((dirxy)) {
            DirXY(horiz: _, vert: 1) => 1,
            DirXY(horiz: 1, vert: 0) => 3,
            DirXY(horiz: -1, vert: 0) => 2,
            _ => 0,
        };
    }

    static Stream<int> makeAnimCycler(Stream<DirXY> dirxyStm, Observable<bool> runningObs) async* {
        var cycling = false;
        var column = 0;
        dirxyStm.listen((dirxy) { cycling = !dirxy.isZero; });
        
        while (true) {
            if (cycling) {
                yield column;
                column = (column + 1) % 4;
            } else {
                column = 0;
            }
            final delay = runningObs.latestVal ? 100 : 200;
            await Future<void>.delayed(Duration(milliseconds: delay));
        }
    }

    /// row = y index; column = x index
    @Mut(["ctx"])
    void _drawSlice(Cctx ctx, int row, int column, num size, GridCC gridcc) {
        final fw = _avatarSheet.width / _horizFrames;
        final fh = _avatarSheet.height / _vertFrames;
        gridcc.drawSlice(_p1posObs.latestVal, _avatarSheet, column, row, fw, fh, size, ctx);
    }

    @override
    @Mut(["ctx"])
    void draw(Cctx ctx, GridCC gridcc) {
        const size = 50;
        final row = _dxyToSlice(_dirxyObs.latestVal);
        _drawSlice(ctx, row, _cycle.latestVal, size, gridcc);
    }
}

class Reticle implements Drawable {
    final Observable<Pos> _p1pob;
    Reticle(this._p1pob);

    @Mut(["ctx"])
    @override
    void draw(Cctx ctx, GridCC gridCC) {
        final color = "#fff".toJS;
        ctx.globalAlpha = 0.5; // semi-transparent
        ctx.strokeStyle = color;
        ctx.fillStyle = color;
        ctx.lineWidth = 1.5;
        final (:xcu, :ycu) = gridCC.cush(_p1pob.latestVal);
        /// outer circle
        ctx.beginPath();
        ctx.arc(xcu, ycu, 6, 0, 2 * pi);
        ctx.stroke();
        /// center dot
        ctx.beginPath();
        ctx.arc(xcu, ycu, 1.5, 0, 2 * pi);
        ctx.fill();
        ctx.globalAlpha = 1.0; // reset
    }
}

class CanvMLeft {
    final Cctx _ctx;
    final int _w;
    final int _h;
    final HTMLElement dispMut; /// must remain mutable so that it can be hidden

    CanvMLeft(this._ctx, this._w, this._h, this.dispMut);

    @factory
    static CanvMLeft create(int w, int h, Iterable<HTMLCanvasElement> stackedCanvs) {
        final canv = HTML.canvas("life", w, h);
        final disp = HTML.div(
            className: "cmlifeparent",
            children: [canv].followedBy(stackedCanvs),
        )..style.minWidth = "${w}px";
        final ctx = canv.getContext('2d') as Cctx;
        return CanvMLeft(ctx, w, h, disp);
    }

    /// Start drawing on p1PosStm events
    @Mut(["this._ctx", "this._disp"])
    void start(Stream<Pos> p1PosStm, Observable<ImmuList<Drawable>> drawItemsObs, Stream<bool> isMergedStm) {
        p1PosStm.listen((p1pos) {
            _frameUpdate(p1pos, _ctx, _w, _h, drawItemsObs.latestVal);
        });
        isMergedStm.listen((isMerged) {
            if (isMerged) {
                dispMut.classList.add("hidden");
            } else {
                dispMut.classList.remove("hidden");
            }
        });
    }

    @Mut(["ctx"])
    static void _frameUpdate(Pos p1pos, Cctx ctx, int w, int h, Iterable<Drawable> drawItems) {
        const scaleLeft = 1.0; /// Left canvas doesn't allow zooming
        final gridcc = GridCC(scaleLeft, p1pos, w, h);
        ctx.clearRect(0, 0, w, h);
        for (final item in drawItems) {
            item.draw(ctx, gridcc);
        }
    }
}

class CanvMRight with Displayable {
    @override
    final Box<HTMLElement> disp;
    final Cctx _ctx;
    final Stream<MouseEvent> click;
    final Stream<MEv> mevStm;
    final int w;
    final int h;

    CanvMRight(this.disp, this._ctx, this.click, this.mevStm, this.w, this.h);
    
    @factory
    static CanvMRight create(int w, int h, Stream<MouseEvent> docMouseUp) {
        final canv = HTML.canvas("hud", w, h);
        /// as per AI recommendation, the mouseUp should be from the doc
        /// in case the cursor leaves the canvas while mouse is down
        final mevStm = makeMouseMoveStm(canv.onMouseDown, canv.onMouseMove, docMouseUp);
        final ctx = canv.getContext('2d') as Cctx;
        final d = Box<HTMLElement>(HTML.div(className: "hudwrap", children: [canv]));
        return CanvMRight(d, ctx, canv.onClick, mevStm, w, h);
    }

    /// Start drawing on p1PosStm events
    void start(Stream<Pos> p1PosStm, Observable<ImmuList<Drawable>> drawItemsObs, Stream<bool> isMergedStm, Observable<double> scaleObs, Observable<Pos> panCenter) {
        p1PosStm.listen((p1pos) {
            _frameUpdate(p1pos, panCenter.latestVal, _ctx, w, h, scaleObs.latestVal, drawItemsObs.latestVal);
        });
    }

    @Mut(["ctx"])
    static void _frameUpdate(Pos p1pos, Pos panCenter, Cctx ctx, int w, int h, double scale, Iterable<Drawable> drawItems) {
        final gridcc = GridCC(scale, panCenter, w, h);
        ctx.clearRect(0, 0, w, h);
        for (final item in drawItems) {
            item.draw(ctx, gridcc);
        }
    }
}

class SimpleOb implements Drawable {
    final Pos pos;
    final HTMLImageElement _img;
    final num _width;
    final num _height;
    final Power? txpower; 
    final bool _visible;

    SimpleOb(this.pos, this._img, this._width, this._height, this.txpower, this._visible);
    
    /// Load image and create instance.
    /// This is a @factory, but dart's static checker doesn't recognize it
    @Eff("http-req")
    static Future<SimpleOb> create(String imgPath, Pos pos, num height, [Power? txpower, bool visible = true]) async {
        final img = await imageload(imgPath);
        return SimpleOb.fromImg(img, pos, height, txpower, visible);
    }

    /// Given a pre-loaded image, create instance.
    @factory
    static SimpleOb fromImg(HTMLImageElement img, Pos pos, num height, [Power? txpower, bool visible = true]) {
        final width = height * img.width / img.height;
        return SimpleOb(pos, img, width, height, txpower, visible);  
    }

    @override
    @Mut(["ctx"])
    void draw(Cctx ctx, GridCC gridcc) {
        if (_visible) {
            gridcc.drawImage(pos, _img, _width, _height, ctx);
        }
    }
}


class Loser {
    static final GC m1LoseThres = GC(40100);
    final OkDialog _dialog;
    HTMLElement disp() => _dialog.disp();

    Loser(this._dialog);
    
    /// On position stream events, check whether the y val is too high.
    @factory
    static Loser create(MissionLogic mlogic, Stream<Pos> p1posStm, E e) {
        final dialog = OkDialog();
        p1posStm
            .where((p1pos) => isCapture(mlogic, p1pos))
            .first
            .then((_) => dialog.showWith("You were captured by enemy scouts!", Consts.msgRtn))
            .then((_) => e.window.open("..", "_self"));
        
        return Loser(dialog);
    }

    /// True if the mission is m1 and the player's y is too high.
    static bool isCapture(MissionLogic mlogic, Pos p1pos) => 
            (mlogic.mission == Mission.m1) && (p1pos.y > m1LoseThres);
}

enum Mission { explore, tutorial, m1 }

@immutable
class MissionLogic {
    final Mission mission;
    final Pos txpos;
    final String msg;
    
    MissionLogic(this.mission, this.txpos, this.msg);
    
    /// The argument should be `window.location.href`.
    @factory
    static MissionLogic create(String href) {
        final mission = _parseMission(href);
        final random = Random();
        final Pos txpos = switch (mission) {
            Mission.explore => Pos(GC(70020), GC(40090)),
            Mission.m1 => Pos(GC(69975 + random.nextInt(50)), GC(40150)),
            Mission.tutorial => Pos(GC(70010), GC(40100)),
        };
        final String msg = switch (mission) {
            Mission.explore => "",
            Mission.m1 => """Use your direction-finding equipment to locate the enemy transmitter.
                
                The adversary's scouts are watching in force. To avoid capture, stay south of the east/west road, which is grid ${Loser.m1LoseThres.val} northing.
                
                Once you have determined the transmitter's grid location, send it to me using your tablet's submission form. Use either an 8 digit grid coordinate (within 10 meters) or a 10 digit grid coordinate (within 1 meter).""",
            Mission.tutorial => "Play the game!",
        };
        return MissionLogic(mission, txpos, msg);
    }

    static Mission _parseMission(String href) {
        final uri = Uri.parse(href);
        return _strToMission(uri.queryParameters["mission"]);
    }

    /// Convert to corresponding `Mission`. Default: `Mission.explore`.
    static Mission _strToMission(String? missionName) { 
        for (final m in Mission.values) {
            if (m.name == missionName) {
                return m;
            }
        }
        print("Invalid mission name '$missionName'. Choices: ${Mission.values}. Defaulting to 'explore' mode.");
        return Mission.explore;
    }
}

class MissionUI with Displayable {
    final MissionLogic mlogic;
    @override
    final Box<HTMLElement> disp;

    MissionUI(this.mlogic, this.disp);

    @factory
    static MissionUI create(MissionLogic mlogic) {
        // final d = switch (mlogic.mission) {
        //     Mission.explore =>  HTML.div(),
        //     Mission.tutorial => _form(),
        //     Mission.m1 => _form(),
        // };
        final disp = HTML.div(); // TODO
        return MissionUI(mlogic, Box(disp));
    }

        
    static Result<Pos, String> parseSubmission(String submission) {
        // final errLen = "Grid coordinates must be entered as 4, 6, 8, or 10 digit grid, according to the mission requirements.\nExample 10 digit grid: 12345 45678";
        final errGeneric = "You must enter two positive numbers separated by one space.\n"
            "Example 10 digit grid: 12345 45678\n"
            "Example 8 digit grid: 1234 8765\n";
        
        /// If `val.$1` and `val.$2` are
        /// positive integers, return them wrapped in `Success`.
        /// Else, return a Failure.
        /// Also checks that lengths match.
        Result<Pos, String> ensurePsvInts((String, String) val) {
            final easting = int.tryParse(val.$1);
            final northing = int.tryParse(val.$2);
            final precision = val.$1.length;
            if (val.$1.length != val.$2.length) { 
                return Failure(errGeneric);
            } else if (easting == null || northing == null) {
                return Failure(errGeneric);
            } else if (easting < 0 || northing < 0) {
                return Failure("Must enter positive values.");
            } else {
                return Success(Pos(GC(easting), GC(northing), precision: precision));
            }
        }

        Result<(T, T), String> mustBeTwo<T>(List<T> x) {
            if (x.length == 2) {
                return Success((x[0], x[1]));
            } else {
                return Failure(errGeneric);
            }
        }

        return submission
            .trim()
            .then((x) => x.split(" "))
            /// If the user accidentally entered two spaces, tolerate that.
            .then((x) => x.where((y) => y.isNotEmpty).toList())
            /// Reject if more/fewer than 2, for example, 123 456 789
            .then((x) => mustBeTwo(x))
            .map((x) => ensurePsvInts(x))
            .then((x) => flatten(x));
    }

    Result<String, String> checkCoords(Pos pos) {
        Result<String, String> successMsg(bool near) {
            if (near) {
                return Success("You successfully located the transmitter behind enemy lines!");
            } else {
                return Failure("Those grid coordinates were incorrect. Try again.");
            }
        }
        final int minimumPrecision;
        if (mlogic.mission == Mission.m1) {
            minimumPrecision = 4; // Must enter 4 or 5 digit grid
        } else {
            minimumPrecision = 3; // We may later change this for other missions
        }
        return pos
            .near(mlogic.txpos, minimumPrecision)
            .map((near) => successMsg(near))
            .then((x) => flatten(x));
    }

    @Mut(["dialog"])
    static void _showAllowGoHome(String succmsg, OkCancelDialog dialog) {
        dialog.showWith(succmsg, Consts.msgRtn, "Continue exploring").then((response) {
            if (response) {
                print('TODO window.open("..", "_self");');
            }
        });
    }

    @Mut(["dialog"])
    void _handleSubmit(String submission, OkCancelDialog dialog, E e) {
        final chk = parseSubmission(submission)
            .map((x) => checkCoords(x))
            .then((x) => flatten(x));
        switch(chk) {
            case Success(val: final succmsg):
                markMissionComplete(mlogic.mission, e);
                _showAllowGoHome(succmsg, dialog);
            case Failure(val: final errmsg):
                dialog.showWith(errmsg);
        }
    }

    static void markMissionComplete(Mission mission, E e) { e.window.localStorage.setItem("lobster_completed_${mission.name}", "true"); }

    HTMLElement _form(E e) {
        final dialog = OkCancelDialog();
        final form = HTML.form()..id = "submit-coords-form";
        final inpEl = HTMLInputElement()
            ..placeholder = "Enter grid coordinates";
        final subbtn = HTML.inputsubmit()
            ..addFlicker(form.onSubmit)
            ..value = "Submit"
            ..className = "game-btn";
        form
            ..appendChild(inpEl)
            ..appendChild(subbtn);
        form.onSubmit.listen((ev) {
            ev.preventDefault();
            _handleSubmit(inpEl.value, dialog, e);
        });
        return HTML.div(children: [form, dialog.disp()]);
    }
}

class Messages with Displayable {
    @override
    final Box<HTMLElement> disp;

    Messages(this.disp);

    @Eff("HTMLAudioElement")
    @factory
    static Messages create(MissionLogic mlogic, Stream<DirXY> dirxyStm) {
        if (mlogic.mission == Mission.explore) {
            return Messages(Box(HTML.div()));  /// Explore doesn't have messages
        }
        /// True if there is an unread message
        final hasMsgSC = StreamController<bool>.broadcast();
        /// True if the messages overlay is shown
        final ovShownSC = StreamController<bool>.broadcast();
        final (msgBtn, msgBtnClick) = _makeMsgBtn(hasMsgSC.stream);
        final (overlay, backBtnClick) = _makeOverlay(ovShownSC.stream, mlogic, hasMsgSC.stream);
        backBtnClick.listen((_) => ovShownSC.add(false));
        _setupAudio(hasMsgSC.stream);
        
        /// Trigger new message after player moves for the first time.
        /// We wait until movement has happened because some browsers disallow
        /// audio playback before page interaction.
        /// The randomness attempts to add realism because your leadership probably
        /// wouldn't contact you at the EXACT INSTANT that you moved :-)
        dirxyStm
            .where((dirxy) => !dirxy.isZero)
            .first
            .then((_) {
                final r = Random();
                final dur = Duration(milliseconds: 500 + r.nextInt(1500));
                Future<void>.delayed(dur).then((_) => hasMsgSC.add(true)); 
            });

        msgBtnClick.listen((_) {
            ovShownSC.add(true);
            hasMsgSC.add(false);
        });

        /// Overlay is initially hidden; no messages initially
        ovShownSC.add(false);
        hasMsgSC.add(false);
        
        final d = HTML.div(ichildren: [msgBtn, overlay]);
        return Messages(Box(d));
    }
    
    @Eff("HTMLAudioElement")
    static void _setupAudio(Stream<bool> hasMsgStm) {
        final notifAudio = HTMLAudioElement();
        hasMsgStm.listen((hasMsg) {
            if (hasMsg) {
                notifAudio.src = "../assets/game_sounds/inc_message.wav";
                notifAudio.loop = true;
                notifAudio.play();
            } else {
                notifAudio.pause();
                notifAudio.currentTime = 0;
            }
        });
    }
    /// Button style depends on whether there is currently an unread message.
    static (Box<HTMLElement>, Stream<Object>) _makeMsgBtn(Stream<bool> hasMsgStm) {
        final messagetext = HTML.p()
            ..className = "fa-regular fa-2x msgs-text";

        final msgbtn = HTML.button()
            ..className = "game-btn msgs-position"
            ..title = "Messages"
            ..appendChild(messagetext);

        hasMsgStm.listen((hasMsg) {
            if (hasMsg) {
                messagetext.classList.add("fa-envelope");
                messagetext.classList.remove("fa-envelope-open");
                msgbtn.classList.add("msgs-unread");
                msgbtn.classList.add("msgs-strobe");
            } else {
                messagetext.classList.add("fa-envelope-open");
                messagetext.classList.remove("fa-envelope");
                msgbtn.classList.remove("msgs-unread");
                msgbtn.classList.remove("msgs-strobe");
            }
        });

        return (Box(msgbtn), msgbtn.onClick);
    }

    /// The returned stream should be used to control the overlay visibility.
    static (Box<HTMLElement>, Stream<MouseEvent>) _makeOverlay(Stream<bool> ovShownStm, MissionLogic mlogic, Stream<bool> hasMsgStm) {
        final backChevron = HTML.p()
            ..className = "fa-solid fa-chevron-left fa-2x msgs-text";
        
        final backBtn = HTML.button()
            ..className = "game-btn backbtn"
            ..appendChild(backChevron);

        final missionMsgText = HTML.p()
            ..id = "mission-message";
        
        hasMsgStm
            .where((hasMsg) => hasMsg)
            .first
            .then((_) => missionMsgText.innerText = mlogic.msg);

        final overlay = HTML.div(id: "overlay", children: [
            backBtn,
            HTML.h2()..innerText = "Messages",
            HTML.span()..className = "fa-solid fa-circle-user fa-3x",
            missionMsgText,
        ]);
        ovShownStm.listen((ovShown) {
            if (ovShown) {
                overlay.classList.remove("hidden");
            } else {
                overlay.classList.add("hidden");
            }
        });
        return (Box(overlay), backBtn.onClick);
    }
}

@immutable
class Objs implements Drawable {
    final ImmuList<SimpleOb> _objs;
    
    Objs(this._objs);
    
    @Eff("http-req")
    @factory
    static Future<Objs> create(Pos playerInitPos, int w, int h) async {
        return Objs(await createBushes(playerInitPos));
    }

    /// randomly-distributed
    @Eff("http-req")
    static Future<ImmuList<SimpleOb>> createBushes(Pos distribCenter) async {
        final random = Random();
        final bush1 = await imageload("../assets/bush_1.png");
        final bush2 = await imageload("../assets/bush_2.png");

        SimpleOb makebush() {
            /// random number not on roads
            Pos rnPosShift() {
                switch (random.nextInt(4)) {
                    case 0: return Pos(
                        GC(499 * random.nextDouble() + -500),
                        GC(499 * random.nextDouble() + -500)
                    );
                    case 1: return Pos(
                        GC(499 * random.nextDouble() + -500),
                        GC(499 * random.nextDouble() + 10)
                    );
                    case 2: return Pos(
                        GC(499 * random.nextDouble() + 10),
                        GC(499 * random.nextDouble() + 10)
                    );
                    case _: return Pos(
                        GC(499 * random.nextDouble() + 10),
                        GC(499 * random.nextDouble() + -500)
                    );
                }
            }
            final height = random.nextInt(6) * 5 + 20;
            final img = random.nextBool() ? bush1 : bush2;
            return SimpleOb.fromImg(img, distribCenter + rnPosShift(), height);
        }

        return ImmuList([for (var i = 0; i < 20000; i++) makebush()]);
    }

    @override
    @Mut(["ctx"])
    void draw(Cctx ctx, GridCC gridcc) {
        for (final obj in _objs.values) {
            final xdiff = (obj.pos.x.val - gridcc.center.x.val).abs();
            final ydiff = (obj.pos.y.val - gridcc.center.y.val).abs();
            if (gridcc.scale < 0.05) {
                return;
            }
            if (xdiff < (30 / gridcc.scale) && ydiff < (30 / gridcc.scale)) {
                obj.draw(ctx, gridcc);
            }
        }
    }
}

class Road implements Drawable {
    Pos coloringCenter;

    Road(this.coloringCenter);

    @override
    void draw(Cctx ctx, GridCC gridcc) {
        /// concrete part of road
        ctx.fillStyle = "#777".toJS;
        gridcc.fillRectCent(coloringCenter + Pos(GC(5), GC(5)), 140 * gridcc.scale, 22000 * gridcc.scale, ctx);
        gridcc.fillRectCent(coloringCenter + Pos(GC(10), GC(5)), 22000 * gridcc.scale, 140 * gridcc.scale, ctx);

        /// dashed center lines
        const yellow = "#f5d742";
        ctx.strokeStyle = yellow.toJS;
        final dashlen = 15 * gridcc.scale;
        final gaplen  = 15 * gridcc.scale;
        ctx.lineWidth = 2.5 * gridcc.scale;
        ctx.setLineDash([dashlen.toJS, gaplen.toJS].toJS);
        /// vertical part
        gridcc.drawLine(
            coloringCenter + Pos(GC(5), GC(-500)),
            coloringCenter + Pos(GC(5), GC(500)),
            ctx,
        );
        /// horizontal part
        gridcc.drawLine(
            coloringCenter + Pos(GC(-500), GC(5)),
            coloringCenter + Pos(GC(500), GC(5)),
            ctx,
        );
        /// reset so other drawing isn't dashed
        ctx.setLineDash(<JSNumber>[].toJS);
    }
}

/// Provides `Drawable`s to the left and right canvas according to 
/// whether they are merged or unmerged.
class LeftRightM {
    Observable<ImmuList<Drawable>> leftObs;
    Observable<ImmuList<Drawable>> rightObs;
    Stream<bool> isMergedStm;
    final HTMLElement _disp;
    
    LeftRightM(this.leftObs, this.rightObs, this._disp, this.isMergedStm);

    /// `rightInitConditional` is shown when the two are separate, and is omitted when the two are merged.
    static LeftRightM create(Iterable<Drawable> leftInit, Iterable<Drawable> rightInitAlways, Iterable<Drawable> rightInitConditional) {
        final lef = ImmuList(leftInit);
        final rig = ImmuList(rightInitAlways.followedBy(rightInitConditional));
        final scleft = StreamController<ImmuList<Drawable>>()..add(lef);
        final scright = StreamController<ImmuList<Drawable>>()..add(rig);
        final scIsMerged = SCoLV.create(false);
        final combicon = HTML.p()
            ..className = "fa-regular fa-object-group fa-2x msgs-text";
        final combbtn = HTML.button()
            ..className = "game-btn comb-position"
            ..title = "Merge view"
            ..appendChild(combicon);
        combbtn.onClick.listen((_) {
            scIsMerged.set(!scIsMerged.latestVal);
        });

        scIsMerged.stream.listen((isMerged) {
            if (isMerged) {
                combicon.classList.add("fa-object-group");
                combicon.classList.remove("fa-object-ungroup");
                scright.add(ImmuList(lef.followedBy(rightInitAlways)));
            }
            else {
                combicon.classList.add("fa-object-ungroup");
                combicon.classList.remove("fa-object-group");
                scright.add(rig);
            }
        });
        
        return LeftRightM(
            Observable(lef, scleft.stream),
            Observable(rig, scright.stream),
            combbtn,
            scIsMerged.stream,
        );
    }

    HTMLElement disp() => _disp;
}

class WalkSfx {
    final HTMLAudioElement _audio;
    bool _playing = false;

    WalkSfx(this._audio);

    void update(bool isMoving, bool isRunning) {
        final rate = isRunning ? 1.6 : 1.0;
        _audio.playbackRate = rate;

        if (isMoving && !_playing) {
            _audio.loop = true;
            _audio.play();
            _playing = true;
        } else if (!isMoving && _playing) {
            _audio.pause();
            _audio.currentTime = 0;
            _playing = false;
        }
    }
}


HTMLElement assembleElems(CanvMLeft cmLife, CanvMRight cmLob, {required Iterable<Displayable> tabletChildren}) {
    final cmLobAndAssociated = HTML.div(
        className: "cmlobparent",
        ichildren: [cmLob.disp].followedBy(tabletChildren.map((el) => el.disp))
    );
    return HTML.div(
        id: "two-canvasses",
        children: [cmLife.dispMut, cmLobAndAssociated]
    );
}


@Eff("*")
void gameMain(Element gamerootelem, E e) async {
    final keydown = document.body!.onKeyDown;
    final keyup = document.body!.onKeyUp;
    final frameStm = makeFrameStm();
    final canvLeftWH = (w: 640, h: 445);
    final canvRightWH = (w: 600, h: 400);
    final mlogic = MissionLogic.create(e.window.location.href);
    final mui = MissionUI.create(mlogic);
    final p1 = PlayerPos.create(Pos(GC(70012), GC(40085)), keydown, keyup, frameStm);
    final phud = PlayerHUD.create(p1.posStm);
    final t1 = await SimpleOb.create("../assets/tx.png", mlogic.txpos, 30, Power(mW: 100), mlogic.mission != Mission.m1);
    final sim = Sim.create(p1.posObs, t1.pos, t1.txpower!);
    final bushes = await Objs.create(p1.posObs.latestVal, canvLeftWH.w, canvLeftWH.h);
    final avatar = await Avatar.create(p1.dirxyStm, p1.runningObs, p1.posObs);
    final reticle = Reticle(p1.posObs);
    final road = Road(p1.posObs.latestVal);
    final zoom = Zoom.create();
    final grid = Grid(zoom.scaleObs);
    final cmLife = CanvMLeft.create(canvLeftWH.w, canvLeftWH.h, []);
    final cmLob = CanvMRight.create(canvRightWH.w, canvRightWH.h, document.body!.onMouseUp);
    final lobc = LOBCol.create(keydown, sim.univLobs, cmLob.click, p1.posObs, zoom.scaleObs);
    final msgs = Messages.create(mlogic, p1.dirxyStm);
    final lrm = LeftRightM.create([road, avatar, bushes, t1], [lobc, grid], [reticle]); 
    final pan = Pan.create(cmLob.mevStm, p1.posObs, p1.posStm, zoom.scaleObs);
    final loser = Loser.create(mlogic, p1.posStm, e);
    final walkAudio = HTMLAudioElement()..src = "../assets/game_sounds/walk_grass.wav";
    final walkSfx = WalkSfx(walkAudio);
    p1.dirxyStm.listen((d) { walkSfx.update(!d.isZero, p1.runningObs.latestVal); });
    cmLife.start(p1.posStm, lrm.leftObs, lrm.isMergedStm);
    cmLob.start(p1.posStm, lrm.rightObs, lrm.isMergedStm, zoom.scaleObs, pan.center);
    gamerootelem.replaceChildren(
        assembleElems(cmLife, cmLob, tabletChildren: 
            [phud, lobc, mui, zoom, msgs]
            /// TODO
            //     pan.disp(),
            //     lrm.disp(),
            //     msgs.disp,
            //     loser.disp(),
            // ])
        )
    );
}
