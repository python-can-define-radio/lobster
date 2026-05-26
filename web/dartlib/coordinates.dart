import 'dart:js_interop';
import 'dart:math' hide e;

import 'package:meta/meta.dart';
import 'package:web/web.dart' show HTMLImageElement;

import './generic.dart';
import './htmlhelp.dart';


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

    bool operator >(GC other) => val > other.val;
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

    /// true if the two positions are near each other.
    Result<bool, String> near(Pos other, int minimumPrecision) {
        final precis = precision;
        if (precis == null) {
            return Failure("Invalid precision. (This is a bug in the program; there is nothing that you as the player can do about it.)");
        } else if (precis < minimumPrecision) {
            return Failure("Your submitted coordinates were not sufficiently precise.\nMust enter at least ${minimumPrecision * 2} digit grid.");
        } else if (precis == 5) {
            final xdiff = (x.val - other.x.val).abs();
            final ydiff = (y.val - other.y.val).abs();
            return Success(xdiff <= 1 && ydiff <= 1);
        } else if (precis == 4) {
            final xdiff = (x.val - (other.x.val/10)).abs();
            final ydiff = (y.val - (other.y.val/10)).abs();
            return Success(xdiff <= 1 && ydiff <= 1);
        } else {
            return Failure("Invalid precision. (This is a bug in the program; there is nothing that you as the player can do about it.)");
        }
    }
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


class Grid implements Drawable {
    final Observable<double> _scaleObs;
    Grid(this._scaleObs);
    @override
    void draw(Cctx ctx, GridCC gridcc) {

        /// Space between gridlines in meters
        final gridUnitSpcExponent = switch(_scaleObs.latestVal) {
            <0.02  => 3,
            <0.2  => 2,
            <2.0  => 1,
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
        
        for (var x = xstart.val; x <= xstop.val; x += gridUnitSpc) {
            gridcc.drawLine(Pos(GC(x), ystart), Pos(GC(x), ystop), ctx);
            gridcc.fillText(
                GC(x).asfivedig,
                Pos(GC(x - charWidth*2.5), ytext),
                ctx
            );
        }
        for (var y = ystart.val; y <= ystop.val; y += gridUnitSpc) {
            gridcc.drawLine(Pos(xstart, GC(y)), Pos(xstop, GC(y)), ctx);
            gridcc.fillText(
                GC(y).asfivedig,
                Pos(xtext, GC(y - charHeight)),
                ctx
            );
        }
    }
}
