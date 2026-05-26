import 'dart:math' hide e;

import 'package:async/async.dart' hide Result;
import 'package:meta/meta.dart';
import 'package:web/web.dart' hide window, document;

import './generic.dart';
import './htmlhelp.dart';
import './coordinates.dart' show GridCC, Pos;




class Zoom with Displayable {
    final Observable<double> scaleObs;
    @override
    final Box<HTMLElement> disp;

    Zoom(this.scaleObs, this.disp);
    
    @factory
    static Zoom create() {
        const initzoom = 1.0;
        final (elem, inoutstm) = makePlusMinus();
        final scaleObs = Observable(initzoom, makeScale(initzoom, inoutstm));
        return Zoom(scaleObs, Box(elem));
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

class Pan with Displayable {
    final Observable<Pos> center;
    @override
    final Box<HTMLElement> disp;

    Pan(this.center, this.disp);

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

        return Pan(pannedCenter.observable, Box(recenterBtn));
    }
}
