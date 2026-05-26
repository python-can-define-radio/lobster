import 'dart:js_interop';
import 'dart:math' hide e;
import 'package:meta/meta.dart';
import 'package:web/web.dart' hide document, window;
import '../dartlib/coordinates.dart';
import '../dartlib/generic.dart';
import '../dartlib/htmlhelp.dart';
import '../dartlib/lobs.dart';




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
