import 'package:web/web.dart';
import '../play/main.dart' show gameMain;
import './htmlhelp.dart' show E;

void main() {
    final Element gamerootelem;
    switch(document.getElementById("gameroot")) {
        case null: throw Exception("Cannot attach elements; no `gameroot` element found");
        case final elem: gamerootelem = elem;
    }
    final e = E(window);
    try {
        gameMain(gamerootelem, e);
    } catch (e) {
        window.alert(e.toString());
        rethrow;
    }
}
