
import 'dart:async';
import 'dart:js_interop';
import 'dart:math' hide e;

import 'package:async/async.dart' hide Result;
import 'package:meta/meta.dart';
import 'package:web/web.dart' hide document, window;

import '../dartlib/coordinates.dart';
import '../dartlib/generic.dart';
import '../dartlib/htmlhelp.dart';



typedef LOB = ({Pos source, Azimuth azimuth, Power rxpow});

class LOBCol with Displayable implements Drawable {
    @override
    final Box<HTMLElement> disp;
    final Observable<ImmuList<LOB>> _lobs;
    /// Selected LOB
    final Observable<LOB?> _sellob;

    LOBCol(this.disp, this._lobs, this._sellob);

    @factory
    static LOBCol create(KbStm keydown, Stream<LOB> univLobs, Stream<MouseEvent> canvclick, Observable<Pos> center, Observable<double> scaleObs) {
        final (lobsStm, lobsCtlUI) = _makeLobStreamAndUI(keydown, univLobs);
        final lobs = Observable(ImmuList<LOB>([]), lobsStm);
        final sellob = _configChosenLOB(lobs, canvclick, center, scaleObs);
        final disp = Box<HTMLElement>(HTML.div(
            children: [makeInfo(lobsStm)],
            ichildren: [lobsCtlUI]
        ));
        return LOBCol(disp, lobs, sellob);
    }

    static HTMLDivElement makeInfo(Stream<ImmuList<LOB>> lobsStm) {
        final lobPowEl = HTML.div()..id = "lob-power";
        lobsStm.listen((lobs) => lobPowEl.innerText = _fmtpow(lobs.values.lastOrNull));
        return lobPowEl;
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
    static (Stream<Object>, Box<HTMLElement>) _makeClear(KbStm keydown) {
        final cDown = keydown.where((ev) => ev.code == "KeyC").asBroadcastStream();
        final cbtn = HTML.button()
            ..addFlicker(cDown)
            ..id = "clear-btn"
            ..className = "game-btn"
            ..innerText = "Clear LOBs [ C ]";
        return (StreamGroup.merge([cDown, cbtn.onClick]), Box(cbtn));
    }

    /// Make Gathering LOBs button and label text.
    /// Also return an observable which is true if the player is currently gathering LOBs.
    static (Observable<bool>, Box<HTMLElement>) _makeGL(KbStm keydown) {
        /// Gathering Lobs SCoLV: false means we are ignoring incoming lobs.
        final isGathSCoLV = SCoLV.create(true);
        final playicon = HTML.p();
        final gatheringLobsBtn = HTML.button()..id = "lob-btn"..className = "game-btn"
            ..appendChild(playicon);

        isGathSCoLV.stream.listen((isGath) {
            const sharedTextClasses = "fa-solid msgs-text";
            if (isGath) {
                playicon.className = "$sharedTextClasses fa-stop col-red";
            } else {
                playicon.className = "$sharedTextClasses fa-play";
            }
        });

        /// Trigger so the initial icon appears
        isGathSCoLV.set(true);

        /// toggle whether player is gathering
        keydown
            .where((ev) => ev.code == "KeyG")
            .listen((_) => isGathSCoLV.set(!isGathSCoLV.latestVal));
        gatheringLobsBtn.onClick.listen((_) => isGathSCoLV.set(!isGathSCoLV.latestVal));
        
        final lobsBtnWithText = HTML.div(id: "lobs-btn-with-text", children: [
            HTML.span()..innerText = "Gathering LOBs [ G ] ", gatheringLobsBtn
        ]);
        return (Observable(true, isGathSCoLV.stream), Box(lobsBtnWithText));
    }

    /// Create a stream of the lobs saved on the simulated DFing equipment,
    /// not to be confused with the stream of lobs coming from the universe.
    /// Also create the elem which controls whether the user is gathering lobs.
    static (Stream<ImmuList<LOB>>, Box<HTMLElement>) _makeLobStreamAndUI(
            KbStm keydown, Stream<LOB> univLobs) {

        final (isGathObs, gathUI) = _makeGL(keydown);
        final (clearStm, clearBtn) = _makeClear(keydown);
        final lobsCtlUI = Box<HTMLElement>(HTML.div(ichildren: [clearBtn, gathUI]));

        Stream<ImmuList<LOB>> makeLobsStm() {
            final curlobs = SCoLV.create(ImmuList<LOB>([]));
            univLobs
                .where((_) => isGathObs.latestVal)
                .listen((lob) {
                    curlobs.set(curlobs.latestVal.followedBy([lob]));
                });

            clearStm.listen((_) {
                curlobs.set(ImmuList([]));
            });
            return curlobs.stream;
        }

        return (makeLobsStm(), lobsCtlUI);
    }

    static String _fmtpow(LOB? lob) {
        final fm = lob?.rxpow.dBm.toStringAsFixed(1);
        return "LOB power: ${fm ?? "__"} dBm\n";
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
