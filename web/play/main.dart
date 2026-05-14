/// LOBSTER Game.
library;


import 'dart:async';
import 'dart:js_interop';
import 'dart:math';
import 'package:async/async.dart' hide Result;
import 'package:meta/meta.dart';
import 'package:web/web.dart';
import '../dartlib/generic.dart';
import '../dartlib/htmlhelp.dart';



class OkCancelDialog {
    final _dialogWrap = HTML.div();

    /// Return value is `true` if user clicked `OK`; `false` if user clicked `Cancel`
    @Mut(["this._dialogWrap"])
    Future<bool> showWith(String msg) {
        final dialog = HTML.dialog()..className = "game-dialog";
        dialog.innerText = msg;
        final okButton = HTML.button()
            ..textContent = 'OK'
            ..classList.add('game-btn');
        final cancelButton = HTML.button()
            ..textContent = 'Cancel'
            ..classList.add('game-btn');
        final buttonRow = HTML.div()
            ..id = 'game-dialog-buttons'
            ..appendChild(okButton)
            ..appendChild(cancelButton);
        final completer = Completer<bool>();
        okButton.onClick.listen((_) {
            dialog.close();
            completer.complete(true);
        });
        cancelButton.onClick.listen((_) {
            dialog.close();
            completer.complete(false);
        });
        dialog.appendChild(buttonRow);
        _dialogWrap.replaceChildren(dialog);
        dialog.show();
        return completer.future;
    }
    HTMLElement disp() => _dialogWrap;
}


/// Grid to Canvas Converter
@immutable
class GridCC {
    /// "grid Meter Base".
    /// An arbitrarily chosen number of pixels
    /// that corresponds to one meter when scale is `1.0`.
    static const gridMB = 22;
    /// Zoom scale. Example: scale = 0.5 would be zoomed out by a factor of 2.
    final double scale;
    final Pos center;
    final int w;
    final int h;
    GridCC(this.scale, this.center, this.w, this.h);
    
    /// Returns (x, y) in canvas units
    static (double, double) cu(Pos p, double scale) => (
        p.x.val * gridMB * scale,
        p.y.val * gridMB * scale,
    );

    static Pos gc(num x, num y, double scale) => Pos(
        GC(x / gridMB / scale),
        GC(y / gridMB / scale),
    );

    /// Given a position (which uses Grid Coordinates),
    /// - converts to canvas units
    /// - shifts based on `center` and the size of the canvas
    /// Returns a pair that is suitable for canvas draw functions.
    ({double xcu, double ycu}) cush(Pos p) {
        final (xcuUnshifted, ycuUnshifted) = cu(p, scale);
        final (xcentcu, ycentcu) = cu(center, scale);
        /// Notice that the vertical formula is inverted 
        /// because canvases use down as positive y direction
        return (
            xcu: xcuUnshifted - xcentcu + w / 2,
            ycu: ycentcu - ycuUnshifted + h / 2,
        );
    }

    /// Fill rectangle, but `p` specifies the center, not the top-left corner.
    @Mut(["ctx"])
    void fillRectCent(Pos p, num wcu, num hcu, Cctx ctx) {
        final (:xcu, :ycu) = cush(p);
        ctx.fillRect(xcu - wcu/2, ycu - hcu/2, wcu, hcu);
    }

    /// `p` specifies the center, not the top-left corner.
    @Mut(["ctx"])
    void drawImage(Pos p, HTMLImageElement img, num wcu, num hcu, Cctx ctx) {
        final (:xcu, :ycu) = cush(p);
        ctx.drawImage(img, xcu - wcu*scale/2, ycu - hcu*scale/2, wcu * scale, hcu * scale);
    }

    @Mut(["ctx"])
    void drawSlice(Pos p, HTMLImageElement img, int column, int row, num fw, num fh, num size, Cctx ctx) {
        final (:xcu, :ycu) = cush(p);
        /// scaled size
        final scsi = max(size * scale, size * 0.6);
        ctx.drawImage(img,
                column * fw, row * fh, fw, fh,
                xcu - scsi/2, ycu - scsi/2, scsi, scsi);
    }

    /// Line from `pos1` to `pos2`
    @Mut(["ctx"])
    void drawLine(Pos pos1, Pos pos2, Cctx ctx) {
        final one = cush(pos1);
        final two = cush(pos2);
        ctx.beginPath();
        ctx.moveTo(one.xcu, one.ycu);
        ctx.lineTo(two.xcu, two.ycu);
        ctx.stroke();
    }

    /// Line from `pos1` to `pos2`
    @Mut(["ctx"])
    void fillText(String text, Pos p, Cctx ctx) {
        final (:xcu, :ycu) = cush(p);
        ctx.fillText(text, xcu, ycu);
    }

    /// Line from `rp1` to `rp2`.
    /// Both are relative to `center`.
    /// Example: given
    ///   - center x is 70030
    ///   - rp1 x is -5
    ///   - rp2 x is 10
    ///   it would draw the line from x=70025 to x=70040.
    /// (The same logic applies for y.)
    @Mut(["ctx"])
    void drawLineRel(Pos rp1, Pos rp2, Cctx ctx) {
        drawLine(center + rp1, center + rp2, ctx);
    }
}

/// Grid Coordinates
@immutable
class GC {
    final num val;
    GC(this.val);
    GC operator +(GC other) => GC(val + other.val);
    GC operator -(GC other) => GC(val - other.val);
    GC operator *(GC other) => GC(val * other.val);
    GC operator /(GC other) => GC(val / other.val);
    String get asfivedig => val.round().toString().padLeft(5, '0');
    
    @override
    bool operator ==(Object other) =>
        switch (other) {
            GC(val: final vo) => val == vo,
            _ => false
        };

    @override
    int get hashCode => val.toInt();
}

@immutable
class Pos {
    final GC x;
    final GC y;
    final int? precision;
    Pos(this.x, this.y, {this.precision});

    Pos operator +(Pos other) =>
        Pos(x + other.x, y + other.y);
    
    @override
    bool operator ==(Object other) =>
        switch (other) {
            Pos(x: final xo, y: final yo)
                => x == xo && y == yo,
            _ => false
        };

    @override
    int get hashCode => (x.val * 1e6 + y.val).toInt();

    Result<bool, String> closeTo(Pos other, int minimumPrecision) {
        final precis = precision;
        if (precis == null) {
            return Failure("Invalid precision. (This is a bug in the program; there is nothing that you as the player can do about it.)");
        } else if (precis < minimumPrecision) {
            return Failure("Your submitted coordinates were not sufficiently precise. Must enter at least ${precis * 2} digit grid.");
        } else if (precis == 5) {
            final xdiff = (x.val - other.x.val).abs();
            final ydiff = (y.val - other.y.val).abs();
            return Success(xdiff <= 1 && ydiff <= 1);
        } else if (precis == 4) {
            final xdiff = (x.val - (other.x.val/10)).abs();
            final ydiff = (y.val - (other.y.val/10)).abs();
            return Success(xdiff <= 1 && ydiff <= 1);
        // } else if (precis == 3) {
        //     return Success(xdiff <= 100 && ydiff <= 100);
        } else {
            return Failure("Invalid precision. (This is a bug in the program; there is nothing that you as the player can do about it.)");
        }
    }
}


sealed class AbleToDraw {
    
}

abstract class Drawable extends AbleToDraw {
    void draw(Cctx ctx, GridCC gridcc);
}

abstract class HasOwnCanv extends AbleToDraw {
    HTMLCanvasElement get canv;
}


@immutable
class DirXY {
    /// 1: moving right; -1: moving left; 0: no horizontal movement
    final int horiz;
    /// 1: moving up; -1: moving down; 0: no vertical movement
    final int vert;

    bool get isZero => horiz == 0 && vert == 0;

    DirXY(this.horiz, this.vert) {
        assert ([1, 0, -1].contains(horiz));
        assert ([1, 0, -1].contains(vert));
    }

    /// Example: if pressing W and A, returns DirXY(-1, 1)
    @factory
    static DirXY fromPressed(ImmuSet<String> pressed) {
        /// subtract bool
        int sb(bool a, bool b) => switch((a, b)) {
            (true, true) => 0,
            (true, false) => 1,
            (false, true) => -1,
            (false, false) => 0
        };
        return DirXY(
            sb(pressed.contains("KeyD"), pressed.contains("KeyA")),
            sb(pressed.contains("KeyW"), pressed.contains("KeyS")),
        );
    }

    /// horiz and vert divided by the magnitude
    (double, double) norm() =>
        sqrt(sq(horiz) + sq(vert))
        .then((mag) =>
            mag == 0.0 ?
            (0.0, 0.0) :
            (horiz / mag, vert / mag)
        );

    @override
    String toString() {
        return "DirXY($horiz, $vert)";
    }
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

class PlayerHUD {
    final Stream<Pos> _posStm;
    PlayerHUD(this._posStm);
    HTMLDivElement disp() {
        final posEl = HTML.div()..id = "player-pos";
        _posStm.listen((pos) =>
            posEl.innerText =
                "grid: 55P DE "
                "${pos.x.asfivedig} "
                "${pos.y.asfivedig}"
        );
        return posEl;
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
        // final x = canvWidth / 2 - size / 2;
        // final y = canvHeight / 2 - size / 2;
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
    final HTMLElement _disp;

    CanvMLeft(this._ctx, this._w, this._h, this._disp);

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
                _disp.classList.add("hidden");
            } else {
                _disp.classList.remove("hidden");
            }
        });
    }

    HTMLElement disp() => _disp;

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

class CanvMRight {
    final HTMLCanvasElement _canv;
    final Stream<MouseEvent> click;
    final Stream<MEv> mevStm;
    final int w;
    final int h;

    CanvMRight(this._canv, this.click, this.mevStm, this.w, this.h);
    
    @factory
    static CanvMRight create(int w, int h, Stream<MouseEvent> docMouseUp) {
        final canv = HTML.canvas("hud", w, h);
        /// as per AI recommendation, the mouseUp should be from the doc
        /// in case the cursor leaves the canvas while mouse is down
        final mevStm = makeMouseMoveStm(canv.onMouseDown, canv.onMouseMove, docMouseUp);
        return CanvMRight(canv, canv.onClick, mevStm, w, h);
    }

    /// Start drawing on p1PosStm events
    void start(Stream<Pos> p1PosStm, Observable<ImmuList<Drawable>> drawItemsObs, Stream<bool> isMergedStm, Observable<double> scaleObs, Observable<Pos> panCenter) {
        final ctx = _canv.getContext('2d') as Cctx;
        p1PosStm.listen((p1pos) {
            _frameUpdate(p1pos, panCenter.latestVal, ctx, w, h, scaleObs.latestVal, drawItemsObs.latestVal);
        });
        isMergedStm.listen((isMerged) {
            if (isMerged) {
                print("Would set canv width and height larger here");
            } else {
                print("opposite");
            }
        });
    }

    ImmuElem disp() => ImmuElem(HTML.div(className: "hudwrap", children: [_canv]));

    @Mut(["ctx"])
    static void _frameUpdate(Pos p1pos, Pos panCenter, Cctx ctx, int w, int h, double scale, Iterable<Drawable> drawItems) {
        final gridcc = GridCC(scale, panCenter, w, h);
        ctx.clearRect(0, 0, w, h);
        for (final item in drawItems) {
            item.draw(ctx, gridcc);
        }
    }
}


class Grid implements Drawable {
    final Observable<double> _scaleObs;
    Grid(this._scaleObs);
    @override
    void draw(Cctx ctx, GridCC gridcc) {

        /// Space between gridlines in meters
        final gridUnitSpcExponent = switch(_scaleObs.latestVal) {
            <0.0099  => 3,
            <0.099  => 2,
            <0.99  => 1,
            _  => 0,
        };

        final gridUnitSpc = pow(10, gridUnitSpcExponent);

        GC toGrid(GC gc) =>
            GC((gc.val / gridUnitSpc).floorToDouble() * gridUnitSpc);

        ctx.strokeStyle = "#ccc".toJS;
        ctx.fillStyle = "#ccc".toJS;
        ctx.lineWidth = 0.5;

        final far = GC(gridUnitSpc * 20);
        final doublefar = far * GC(2);
        final xstart = toGrid(gridcc.center.x - far);
        final xstop = xstart + doublefar;
        final ystart = toGrid(gridcc.center.y - far);
        final ystop = ystart + doublefar;
        final xtext = gridcc.center.x - GC(13.6 / gridcc.scale);
        final ytext = gridcc.center.y + GC(8.7 / gridcc.scale);

        /// this is an empirical guess. Eventually we should use a monospace
        /// font and fetch the width of it if possible.
        final charWidth = 0.3 / gridcc.scale;
        /// see note on charWidth
        final charHeight = 0.2 / gridcc.scale;

        String lastDigits(GC gc) {
            final numdig = (gridUnitSpcExponent + 2).clamp(2, 5);
            return gc.asfivedig.substring(5 - numdig, 5);
        }
        
        for (var x = xstart.val; x <= xstop.val; x += gridUnitSpc) {
            gridcc.drawLine(Pos(GC(x), ystart), Pos(GC(x), ystop), ctx);
            gridcc.fillText(
                lastDigits(GC(x)),
                Pos(GC(x - charWidth), ytext),
                ctx
            );
        }
        for (var y = ystart.val; y <= ystop.val; y += gridUnitSpc) {
            gridcc.drawLine(Pos(xstart, GC(y)), Pos(xstop, GC(y)), ctx);
            gridcc.fillText(
                lastDigits(GC(y)),
                Pos(xtext, GC(y - charHeight)),
                ctx
            );
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

typedef LOB = ({Pos source, Azimuth azimuth, Power rxpow});


class LOBCol implements Drawable {
    final ImmuElem _gatheringLobsBtn;
    final ImmuElem _clearBtn;
    final Stream<ImmuList<LOB>> _lobsStm;
    final Observable<ImmuList<LOB>> _lobs;
    /// Selected LOB
    final Observable<LOB?> _sellob;

    LOBCol(this._gatheringLobsBtn, this._clearBtn, this._lobsStm, this._lobs, this._sellob);

    @factory
    static LOBCol create(KbStm keydown, Stream<LOB> univLobs, Stream<MouseEvent> canvclick, Observable<Pos> center, Observable<double> scaleObs) {
        final (clear, clearBtn) = _configClearing(keydown);
        final (lobsStm, gatheringLobsBtn) = _makeLobStream(clear, keydown, univLobs);
        final lobs = Observable(ImmuList<LOB>([]), lobsStm);
        final sellob = _configChosenLOB(lobs, canvclick, center, scaleObs);
        return LOBCol(gatheringLobsBtn, clearBtn, lobsStm, lobs, sellob);
    }

    static Observable<LOB?> _configChosenLOB(Observable<ImmuList<LOB>> lobs, Stream<MouseEvent> canvclick, Observable<Pos> center, Observable<double> scaleObs) {
        final sc = StreamController<LOB?>();
        // canvclick.listen((ev) {
        //     final gridcc = GridCC(scaleObs.latestVal, center.latestVal);
        //     final chosen = decideClosest(lobs.latestVal, gridcc, ev);
        //     print("Selected lob: $chosen");
        //     sc.add(chosen);
        // });
        return Observable(null, sc.stream);
    }
    
    // static LOB? decideClosest(ImmuList<LOB> immulobs, GridCC gridcc, MouseEvent ev) {
    //     window.alert("${gridcc.cush(gridcc.center)}");
    //     final lobs = immulobs.values;
    //     final shiftx = p1pos.xcu + ev.offsetX - canvWidth / 2;
    //     final shifty = p1pos.ycu + ev.offsetY - canvHeight / 2;
    //     num dist(LOB lob) {
    //         final dx = (lob.source.xcu - shiftx).abs();
    //         final dy = (lob.source.ycu - shifty).abs();
    //         return dx + dy;
    //     }
    //     lobs.sort((a, b) => dist(a).compareTo(dist(b)));
    //     final near = lobs.where((lob) => dist(lob) < 40);
    //     return near.firstOrNull;
    // }
    
    /// Returns a stream and the clear button.
    /// Events in the stream (both clicks and keypresses) should cause a clear.
    static (Stream<Object>, ImmuElem) _configClearing(KbStm keydown) {
        final cDown = keydown.where((ev) => ev.code == "KeyC").asBroadcastStream();
        final cbtn = HTML.button()
            ..addFlicker(cDown)
            ..id = "clear-btn"
            ..className = "game-btn"
            ..innerText = "Clear LOBs [ C ]";
        return (StreamGroup.merge([cDown, cbtn.onClick]), ImmuElem(cbtn));
    }

    /// Create a stream of the lobs saved on the simulated DFing equipment,
    /// not to be confused with the stream of lobs coming from the universe.
    /// Also create the checkbox which controls whether the user is gathering lobs.
    static (Stream<ImmuList<LOB>>, ImmuElem) _makeLobStream(
            Stream<Object> clear, KbStm keydown, Stream<LOB> univLobs) {
        /// Gathering Lobs SCoLV
        final gLSCoLV = SCoLV.create(true);
        final playtext = HTML.p()
            ..className = "fa-solid fa-stop msgs-text col-red";
        final gatheringLobsBtn = HTML.button()..id = "lob-btn"..className = "game-btn"
            ..appendChild(playtext);

        gLSCoLV.stream.listen((play) {
        if (play) {
                playtext.classList.add("fa-stop");
                playtext.classList.remove("fa-play");
                playtext.classList.add("col-red");
            }
            else {
                playtext.classList.add("fa-play");
                playtext.classList.remove("fa-stop");
                playtext.classList.remove("col-red");
            }
        });


        keydown
            .where((ev) => ev.code == "KeyG")
            .listen((_) => gLSCoLV.set(!gLSCoLV.latestVal));
        gatheringLobsBtn.onClick.listen((_) => gLSCoLV.set(!gLSCoLV.latestVal));

        final filtlobs = univLobs.where((_) => gLSCoLV.latestVal);
        
        final curlobs = SCoLV.create(ImmuList<LOB>([]));
        filtlobs.listen((lob) {
            curlobs.set(curlobs.latestVal.followedBy([lob]));
        });
        clear.listen((_) {
            curlobs.set(ImmuList([]));
        });
        
        return (curlobs.stream, ImmuElem(gatheringLobsBtn));
    }

    static String _fmtpow(LOB? lob) {
        final fm = lob?.rxpow.dBm.toStringAsFixed(1);
        return "LOB power: ${fm ?? "__"} dBm\n";
    }

    HTMLDivElement dispInfo() {
        final lobPowEl = HTML.div()..id = "lob-power";
        _lobsStm.listen((lobs) => lobPowEl.innerText = _fmtpow(lobs.values.lastOrNull));
        return lobPowEl;
    }

    HTMLDivElement dispCtl() {
        final globtext = ImmuElem(HTML.span()..innerText = "Gathering LOBs [ G ] ");
        final lobsBtnWithText = ImmuElem(HTML.div(id: "lobs-btn-with-text", ichildren: [
            globtext,
            _gatheringLobsBtn,
        ]));
        return HTML.div(ichildren: [_clearBtn, lobsBtnWithText]);
    }

    @Mut(["ctx"])
    static void _drawOne(Cctx ctx, GridCC gridcc, LOB lob, String color) {
        const arbitrarilyLargeLobLength = 1000;
        final dest = lob.source + Pos(
            GC(arbitrarilyLargeLobLength * lob.azimuth.cosresult),
            GC(arbitrarilyLargeLobLength * lob.azimuth.sinresult)
        );
        ctx.lineWidth = 2;
        ctx.strokeStyle = color.toJS;
        gridcc.drawLine(lob.source, dest, ctx);
    }

/* 

The following is a collection of tips to improve canvas performance.
Pre-render similar primitives or repeating objects on an offscreen canvas

If you find yourself repeating some of the same drawing operations on each animation frame, consider offloading them to an offscreen canvas. You can then render the offscreen image to your primary canvas as often as needed, without unnecessarily repeating the steps needed to generate it in the first place.

myCanvas.offscreenCanvas = document.createElement("canvas");
myCanvas.offscreenCanvas.width = myCanvas.width;
myCanvas.offscreenCanvas.height = myCanvas.height;

myCanvas.getContext("2d").drawImage(myCanvas.offScreenCanvas, 0, 0);
*/


    /// Same as plain `draw()` but the dependencies are explicit.
    @Mut(["ctx"])
    static void drawStatic(Iterable<LOB> lobs, LOB? sellob, Cctx ctx, GridCC gridcc) {
        void drawOne(LOB lob, String color) => _drawOne(ctx, gridcc, lob, color);
        ctx.globalAlpha = 0.5;
        for (final lob in withoutLast(lobs)) {
            drawOne(lob, "orange");
        }
        ctx.globalAlpha = 1.0;
        lobs.lastOrNull?.then((lob) => drawOne(lob, "red"));
        sellob?.then((lob) => drawOne(lob, "blue"));
    }

    @override
    @Mut(["ctx"])
    void draw(Cctx ctx, GridCC gridcc) {
        drawStatic(_lobs.latestVal.values, _sellob.latestVal, ctx, gridcc);
    }
}


class Azimuth {
    final double sinresult;
    final double cosresult;

    Azimuth(this.sinresult, this.cosresult);

    /// Given the player (receiver) position and the transmitter position
    /// compute the azimuth from the player's perspective.
    @factory
    static Azimuth fromPositions(Pos p1pos, Pos txpos) {
        final xd = txpos.x.val - p1pos.x.val;
        final yd = txpos.y.val - p1pos.y.val;
        final dist = sqrt(xd * xd + yd * yd);
        return Azimuth(yd / dist, xd / dist);
    }
}


class Power {
    final double mW;
    double get dBm => 10 * logbase10(mW);
    Power({required this.mW});
    Power operator *(double other) {
        return Power(mW: mW * other);
    }
}

/// Simulator. A class that simulates LOBs.
class Sim {
    /// LOBs coming from the universe (as opposed to those which we have gathered)
    final Stream<LOB> univLobs;
    
    Sim(this.univLobs);
    
    /// p1pob: Player 1 Position Observable
    @factory
    static Sim create(Observable<Pos> p1pob, Pos txpos, Power txpower) {
        final random = Random();
        final univLobs = Stream<Null>.periodic(Duration(milliseconds: 50))
                .where((_) => random.nextInt(5) == 0)
                .map((_) => _makelob(p1pob.latestVal, txpos, txpower, random))
                .asBroadcastStream();
        return Sim(univLobs);
    }

    static LOB _makelob(Pos p1pos, Pos txpos, Power txpower, Random random) => (
        source: p1pos,
        azimuth: _noi(Azimuth.fromPositions(p1pos, txpos), random),
        rxpow: _distLoss(p1pos, txpos, txpower, random),
    );

    /// add random noise. Need to figure out whether this is typical distribution
    static Azimuth _noi(Azimuth a, Random random) {
        p3(double x) => x * x * x;
        return Azimuth(
            a.sinresult + 0.003 * p3(6 * (random.nextDouble() - 0.5)),
            a.cosresult + 0.003 * p3(6 * (random.nextDouble() - 0.5)),
        );
    }

    /// A very rudimentary path loss computation
    static Power _distLoss(Pos p1pos, Pos txpos, Power txpower, Random random) {
        final xd = txpos.x.val - p1pos.x.val;
        final yd = txpos.y.val - p1pos.y.val;
        final dist = sqrt(xd * xd + yd * yd);
        return txpower * 0.1 * (1 / sq(dist)) * (random.nextDouble() * 0.1 + 0.9);
    }
}

enum Mission { explore, tutorial, m1 }

class MissionLogic {
    final Mission mission;
    final Pos txpos;
    
    MissionLogic(this.mission, this.txpos);
    
    /// The argument should be `window.location.href`.
    @factory
    static MissionLogic create(String href) {
        final mission = _parseMission(href);
        final random = Random();
        final txpos = Pos(GC(69975 + random.nextInt(50)), GC(40150));
        return MissionLogic(mission, txpos);
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


class MissionUI {
    final _dialog = OkCancelDialog();
    final MissionLogic mlogic;

    MissionUI(this.mlogic);

    @Eff("window.open")
    HTMLElement disp() => 
        switch (mlogic.mission) {
            Mission.explore =>  HTML.div(),
            Mission.tutorial => _form(),
            Mission.m1 => _form(),
        };
        
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
        Result<String, String> successMsg(bool close) {
            if (close) {
                return Success("Correct!");
            } else {
                return Failure("Those grid coordinates were incorrect. Try again. DEBUG: real coords are ${mlogic.txpos.x.val} ${mlogic.txpos.y.val}");
            }
        }
        final int minimumPrecision;
        if (mlogic.mission == Mission.m1) {
            minimumPrecision = 4; // Must enter 4 or 5 digit grid
        } else {
            minimumPrecision = 3; // We may later change this for other missions
        }
        return pos
            .closeTo(mlogic.txpos, minimumPrecision)
            .map((close) => successMsg(close))
            .then((x) => flatten(x));
    }

    @Eff("window.open")
    void _showAllowGoHome(String msg) {
        _dialog.showWith(msg).then((response) {
            if (response) {
                window.open("..", "_self");
            }
        });
    }

    @Eff("window.open")
    void _handleSubmit(String submission) {
        final chk = parseSubmission(submission)
            .map((x) => checkCoords(x))
            .then((x) => flatten(x));
        switch(chk) {
            case Success(val: final succmsg):
                _showAllowGoHome(succmsg);
            case Failure(val: final errmsg):
                _dialog.showWith(errmsg);
        }
    }

    @Eff("window.open")
    HTMLFormElement _form() {
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
        form.onSubmit.listen((e) {
            e.preventDefault();
            Future.delayed(Duration(milliseconds: 1), () => _handleSubmit(inpEl.value));
        });
        return form;
    }
    /// The result of submitting the form
    HTMLElement dispResult() => _dialog.disp();
}


HTMLElement assembleElems(CanvMLeft cmLife, CanvMRight cmLob, {required Iterable<HTMLElement> tabletChildren}) {
    final cmLobAndAssociated = HTML.div(
        className: "cmlobparent",
        ichildren: [cmLob.disp()].followedBy(tabletChildren.map((el) => ImmuElem(el)))
    );
    return HTML.div(
        id: "two-canvasses",
        children: [cmLife.disp(), cmLobAndAssociated]
    );
}


class Zoom {
    final Observable<double> scaleObs;
    final HTMLElement _dispElem;

    Zoom(this.scaleObs, this._dispElem);
    
    @factory
    static Zoom create() {
        const initzoom = 1.0;
        final (elem, inoutstm) = makePlusMinus();
        final scaleObs = Observable(initzoom, makeScale(initzoom, inoutstm));
        return Zoom(scaleObs, elem);
    }

    /// Return a stream of the current zoom level.
    /// The stream `stm` controls the output stream:
    ///   `true` => zoom in by a factor of 2
    ///   `false` => zoom out by a factor of 2
    /// Example:
    ///  If initzoom = 1 and stm produces [true, false, false, true],
    /// then the output stream would be 1, 2, 1, 0.5, 1.
    static Stream<double> makeScale(double initzoom, Stream<bool> inoutstm) {
        return inoutstm.scan<double>(
            initzoom,
            (prev, zoomIn) => zoomIn ?
              prev * 2 :
              max(prev / 2, 0.005),
        );
    }

    HTMLElement disp() => _dispElem;
    
    static (HTMLElement, Stream<bool>) makePlusMinus() {
        final zoomintext = HTML.p()
            ..className = "fa-solid fa-magnifying-glass-plus fa-2x msgs-text";

        final zoomouttext = HTML.p()
            ..className = "fa-solid fa-magnifying-glass-minus fa-2x msgs-text";

        final zoomin = HTML.button()
            ..className = "game-btn"
            ..id = "zoomin"
            ..title = "Zoom in"
            ..appendChild(zoomintext);

        final zoomout = HTML.button()
            ..className = "game-btn"
            ..id = "zoomout"
            ..title = "Zoom out"
            ..appendChild(zoomouttext);

        final wrapperdiv = HTML.div()
            ..appendChild(zoomin)
            ..appendChild(zoomout);

        final inoutstm = combineInOut(zoomin.onClick, zoomout.onClick);
        return (wrapperdiv, inoutstm);
    }

    static Stream<bool> combineInOut(Stream<Object> instm, Stream<Object> outstm) {
        final t = instm.map((_) => true);
        final f = outstm.map((_) => false);
        return StreamGroup.merge([t, f]);
    }
}


class Pan {
    final Observable<Pos> center;
    final HTMLButtonElement _resetBtn;

    Pan(this.center, this._resetBtn);

    @factory
    static Pan create(Stream<MEv> mevStm, Observable<Pos> p1pob, Stream<Pos> p1stm, Observable<double> scaleObs) {
        final pannedCenter = SCoLV.create(p1pob.latestVal);
        final recenterBtn = HTML.button()..innerText = "Re-center"..id = "recenter-btn"..className = "game-btn hidden";
        final followPlayer = SCoLV.create(true);

        p1stm.listen((p) {
            if (followPlayer.latestVal) {
                pannedCenter.set(p);
            }
        });

        mevStm.where((mev) => mev.isDown).listen((mev) {
            final diff = GridCC.gc(-mev.dx, mev.dy, scaleObs.latestVal);
            pannedCenter.set(pannedCenter.latestVal + diff);
            followPlayer.set(false);
        });

        followPlayer.stream.listen((follow) {
            if (follow) {
                recenterBtn.classList.add("hidden");
            } else {
                recenterBtn.classList.remove("hidden");
            }
        });

        recenterBtn.onClick.listen((_) {
            pannedCenter.set(p1pob.latestVal);
            followPlayer.set(true);
        });

        return Pan(pannedCenter.observable, recenterBtn);
    }

    HTMLElement disp() => _resetBtn;
}

class Messages {
    final HTMLElement _msgBtn;
    final HTMLElement _overlay;

    Messages(this._msgBtn, this._overlay);

    @factory
    static Messages create(bool msgBtnVisible) {
        /// True if there is an unread message
        final incmsgSC = StreamController<bool>.broadcast();
        /// True if the messages overlay is shown
        final shownSC = StreamController<bool>.broadcast(); 
        final msgBtn = _makeMsgBtn(incmsgSC.stream, msgBtnVisible);
        final (overlay, backBtnClick) = _makeOverlay(shownSC.stream);
        backBtnClick.listen((_) => shownSC.add(false));
        incmsgSC.add(true);
        shownSC.add(false);
        msgBtn.onClick.listen((_) {
            shownSC.add(true);
            incmsgSC.add(false);
        });
        return Messages(msgBtn, overlay);
    }
    
    static HTMLElement _makeMsgBtn(Stream<bool> incMsgStm, bool msgBtnVisible) {
        final messagetext = HTML.p()
            ..className = "fa-regular fa-2x msgs-text";

        final msgbtn = HTML.button()
            ..className = "game-btn msgs-position"
            ..title = "Messages"
            ..appendChild(messagetext);

        if (!msgBtnVisible) {
            msgbtn.classList.add("hidden");
        }

        incMsgStm.listen((incMsg) {
            if (incMsg) {
                messagetext.classList.add("fa-envelope");
                messagetext.classList.remove("fa-envelope-open");
                msgbtn.classList.add("msgs-unread");
            }
            else {
                messagetext.classList.add("fa-envelope-open");
                messagetext.classList.remove("fa-envelope");
                msgbtn.classList.remove("msgs-unread");
            }
        });

        return msgbtn;
    }

    HTMLElement dispMsgBtn() => _msgBtn;

    static (HTMLElement, Stream<MouseEvent>) _makeOverlay(Stream<bool> shownStm) {
        final backChevron = HTML.p()
            ..className = "fa-solid fa-chevron-left fa-2x msgs-text";
        
        final backBtn = HTML.button()
            ..className = "game-btn backbtn"
            ..appendChild(backChevron);

        final overlay = HTML.div(id: "overlay", children: [
            backBtn,
            HTML.h2()..innerText = "Messages",
            HTML.span()..className = "fa-solid fa-circle-user fa-3x",
            HTML.p()
                ..id = "m1-message"
                ..innerText = """The adversary's scouts are watching in force.

                To avoid capture, stay behind the FLOT -- don't go any further North than grid 40100 northing.
                
                Once you have determined the transmitter's grid location, send it to me using your tablet's submission form using either an 8 digit grid coordinate (within 10 meters) or a 10 digit grid coordinate (within 1 meter).""",
        ]);
        shownStm.listen((shown) {
            if (shown) {
                overlay.classList.remove("hidden");
            }
            else {
                overlay.classList.add("hidden");
            }
        });
        return (overlay, backBtn.onClick);
    }

    HTMLElement dispoverlay() => _overlay; 
}

/*
TODO after releasing version 2
@immutable
class Objs implements HasOwnCanv {
    @override
    final HTMLCanvasElement canv;

    // in create, use something like this:
        final canv = HTML.canvas("life", w, h);
        final ctx = canv.getContext('2d') as Cctx;
        .drawInProgress(ctx, GridCC(1, playerInitPos, w, h));


    @Mut(["ctx"])
    void drawInProgress(Cctx ctx, GridCC gridcc) {
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

 */

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
    HTMLImageElement img;

    Road(this.coloringCenter, this.img);

    @Eff("http-req")
    @factory
    static Future<Road> create(Pos coloringCenter) async {
        final img = await imageload("../assets/groundtiles.png");
        return Road(coloringCenter, img);
    }

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


@Eff("*")
void main() async {
    final keydown = document.body!.onKeyDown;
    final keyup = document.body!.onKeyUp;
    final frameStm = makeFrameStm();
    final canvLeftWH = (w: 640, h: 445);
    final canvRightWH =  (w: 600, h: 400);
    final mlogic = MissionLogic.create(window.location.href);
    final mui = MissionUI(mlogic);
    final p1 = PlayerPos.create(Pos(GC(70012), GC(40085)), keydown, keyup, frameStm);
    final phud = PlayerHUD(p1.posStm);
    final t1 = await SimpleOb.create("../assets/tx.png", mlogic.txpos, 30, Power(mW: 100), mlogic.mission != Mission.m1);
    final sim = Sim.create(p1.posObs, t1.pos, t1.txpower!);
    final bushes = await Objs.create(p1.posObs.latestVal, canvLeftWH.w, canvLeftWH.h);
    final avatar = await Avatar.create(p1.dirxyStm, p1.runningObs, p1.posObs);
    final reticle = Reticle(p1.posObs);
    final road = await Road.create(p1.posObs.latestVal);
    final zoom = Zoom.create();
    final grid = Grid(zoom.scaleObs);
    final cmLife = CanvMLeft.create(canvLeftWH.w, canvLeftWH.h, []);
    final cmLob = CanvMRight.create(canvRightWH.w, canvRightWH.h, document.body!.onMouseUp);
    final lobc = LOBCol.create(keydown, sim.univLobs, cmLob.click, p1.posObs, zoom.scaleObs);
    final msgs = Messages.create(mlogic.mission == Mission.m1);
    final lrm = LeftRightM.create([road, avatar, bushes, t1], [lobc, grid], [reticle]); 
    final pan = Pan.create(cmLob.mevStm, p1.posObs, p1.posStm, zoom.scaleObs);
    cmLife.start(p1.posStm, lrm.leftObs, lrm.isMergedStm);
    cmLob.start(p1.posStm, lrm.rightObs, lrm.isMergedStm, zoom.scaleObs, pan.center);
    switch(document.getElementById("gameroot")) {
        case null: window.alert("Cannot attach elements; no `gameroot` element found");
        case final gamerootelem: gamerootelem.replaceChildren(
            assembleElems(cmLife, cmLob, tabletChildren: [
                phud.disp(),
                lobc.dispInfo(),
                lobc.dispCtl(),
                mui.disp(),
                mui.dispResult(),
                zoom.disp(),
                pan.disp(),
                lrm.disp(),
                msgs.dispMsgBtn(),
                msgs.dispoverlay(),
            ])
        );
    }
}
