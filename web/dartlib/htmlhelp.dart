import 'dart:async';
import 'dart:js_interop';
import 'package:web/web.dart';
import 'generic.dart' show Mut, Eff;
import 'dart:math';


typedef Cctx = CanvasRenderingContext2D;
typedef KbStm = ElementStream<KeyboardEvent>;
typedef DuStm = Stream<Duration>;


/// Methods for creating HTML elems
class HTML {
    static HTMLButtonElement button() =>
            document.createElement('button') as HTMLButtonElement;
    static HTMLDialogElement dialog() =>
            document.createElement('dialog') as HTMLDialogElement;
    static HTMLFormElement form() =>
            document.createElement('form') as HTMLFormElement;
    static HTMLHeadingElement h2() =>
            document.createElement("h2") as HTMLHeadingElement;    
    static HTMLParagraphElement p() =>
            document.createElement('p') as HTMLParagraphElement;    
    static HTMLSpanElement span() =>
            document.createElement('span') as HTMLSpanElement;
    static HTMLCanvasElement canvas(String className, int w, int h) {
        return (document.createElement('canvas') as HTMLCanvasElement)
                ..className = className
                ..width = w
                ..height = h;
    }
    static HTMLInputElement checkbox() {
        final el = document.createElement('input') as HTMLInputElement;
        el.setAttribute("type", "checkbox");
        return el;
    }
    static HTMLDivElement div({String? id, String? className, Iterable<HTMLElement>? children, Iterable<Box<HTMLElement>>? ichildren}) {
        final e = document.createElement('div') as HTMLDivElement;
        if (id != null) {
            e.id = id;
        }
        if (className != null) {
            e.className = className;
        }
        if (children != null) {
            for (final c in children) {
                e.appendChild(c);
            }
        }
        if (ichildren != null) {
            for (final ic in ichildren) {
                e.appendChild(ic.privateElemDoNotTouchInFilesOutsideHtmlHelp
              );
            }
        }
        return e;
    }
    static HTMLInputElement inputsubmit() {
        final el = document.createElement('input') as HTMLInputElement;
        el.setAttribute("type", "submit");
        return el;
    }
}


extension Flickerable on HTMLElement {
    /// Adds "button-active" to the classList, and then removes it `milliseconds` ms later.
    /// Debugging note: this assumes that...
    /// - the element still exists after `milliseconds`
    /// - nothing else is adding/removing the "button-active" class
    @Mut(["this.classList"])
    void addFlicker(Stream<Object> stm, [int milliseconds = 100]) {
        stm.listen((_) {
            classList.add("button-active");
            Future.delayed(Duration(milliseconds: milliseconds), () => classList.remove("button-active"));
        });
    }
}

class Box<T> {
    final HTMLElement privateElemDoNotTouchInFilesOutsideHtmlHelp;

    Box(this.privateElemDoNotTouchInFilesOutsideHtmlHelp);
}


/// Repeatedly call requestAnimationFrame; pass the time delta as an argument to `frameUpdate`
@Eff("window.requestAnimationFrame")
void runEachFrame(void Function(Duration) frameUpdate) {
    void dartRAF(void Function(double) callback) {
        window.requestAnimationFrame(callback.toJS);
    }

    double tlast = 0;
    void animate(double timems) {
        final deltams = timems - tlast;
        tlast = timems;
        frameUpdate(Duration(milliseconds: deltams.toInt()));
        dartRAF(animate);
    }

    dartRAF(animate);
}

/// Run requestAnimationFrame repeatedly. Return a stream of time differences between frames. 
@Eff("window.requestAnimationFrame")
Stream<Duration> makeFrameStm() {
    final timeDiffSC = StreamController<Duration>();
    runEachFrame((Duration tdelta) => timeDiffSC.add(tdelta));
    return timeDiffSC.stream.asBroadcastStream();
}

/// A custom Mouse Event record for use with makeMouseMoveStm because I don't like how JS handles mouse events.
typedef MEv = ({bool isDown, num dx, num dy});

/// Given the mouseDown, mouseMove, and mouseUp streams from the browser,
/// return a stream of `MEv`. Example:
/// Mouse is not clicked. Mouse moves from 30, 90 to 35, 92.
/// This stream will contain (isdown: false, dx: 5, dy: 2).
/// Mouse is clicked. Mouse moves to 37, 100.
/// This stream will contain (isdown: true, dx: 2, dy: 8).
Stream<MEv> makeMouseMoveStm(Stream<MouseEvent> mouseDown, Stream<MouseEvent> mouseMove, Stream<MouseEvent> mouseUp) {
    final sc = StreamController<MEv>();
    ({num x, num y})? prev;
    var isDown = false;
    mouseDown.listen((e) {
        isDown = true;
        prev = (x: e.clientX, y: e.clientY);
    });
    mouseUp.listen((_) {
        isDown = false;
        prev = null;
    });
    mouseMove.listen((e) {
        final p = prev;
        // First move after down OR first ever move → no delta
        if (p == null) {
            prev = (x: e.clientX, y: e.clientY);
            return;
        }
        final dx = e.clientX - p.x;
        final dy = e.clientY - p.y;
        prev = (x: e.clientX, y: e.clientY);
        sc.add((isDown: isDown, dx: dx, dy: dy));
    });
    return sc.stream.asBroadcastStream();
}


@Mut(["ctx"])
void fillCircle(num x, num y, num radius, Cctx ctx) {
    ctx.beginPath();
    ctx.arc(x, y, radius, 0, 2 * pi);
    ctx.fill();
}

/// Load an image. Wait for it to decode before returning.
@Eff("http-req")
Future<HTMLImageElement> imageload(String path) async {
    final el = HTMLImageElement()..src = path;
    await el.decode().toDart;
    return el;
}
