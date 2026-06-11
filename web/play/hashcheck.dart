import 'dart:math';
import 'package:test/test.dart';


List<String> splitStringBasedOnStuff(String orig) {
    return [orig.substring(0, 3), orig.substring(3)];
}

void runWithRandInts<T>(T Function(int) f, Matcher matcher, int max) {
    final r = Random();
    final ri = r.nextInt(max);
    expect(f(ri), matcher);
}


void main() {
    // test('.split() splits the string on the delimiter', () {
    //     var string = 'foo,bar,baz';
    //     expect(string.split(','), equals(['foo', 'bar', 'baz']));
    // });

    
    test("something", () {
        runWithRandInts((x) => x + 1, 3, 5);
    });

    // test('.trim() removes surrounding whitespace', () {
    //   var string = '  foo ';
    //   expect(string.trim(), equals('foo'));
    // });
}


