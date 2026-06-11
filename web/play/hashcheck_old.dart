import 'dart:io';
import 'dart:convert';
import 'package:crypto/crypto.dart';










/// TODO: NOT YET REVERIFIED.
/// Starting at the index `start` in `text`, search for the matching closing brace. Handles nested.
/// Examples:
/// findMatchingBrace("abc(de)fg(hi)", 2, "(", ")") == 6
/// findMatchingBrace("abc(de)fg(hi)", 7, "(", ")") == 12
/// side-eff: no
int? findMatchingBrace(String text, int start, String startc, String endc) {
  int depth = 0;
  for (int i = start; i < text.length; i++) {
    if (text[i] == startc) depth++;
    if (text[i] == endc) {
      depth--;
      if (depth == 0) return i;
    }
  }
  return null;
}

typedef Chunk = ({
  String? hashline,
  String matchRest,
  // String storedHash,
  // String computedHash,
});


/// TODO: I broke this. -- J
/// Given `text` (which is assumed to be syntactically valid Dart source code),
/// search verified hashes. Return the list of those which do not match the hash
/// up to the matching "}".
/// side-eff: no
List<VerificationFailure> extractAndVerifyBlocks(String text) {
  /// intentionally separated so that it doesn't get found when searching this file
  const marker = "/// vhash: ";

  final starts = Iterable<int>.generate(text.length)
      .where((i) => text.startsWith(marker, i))
      .map((i) => i + marker.length);

  return starts.map((start) {
    final lineEnd = text.indexOf("\n", start);
    final storedHash = text.substring(start, lineEnd);
    // final colonIndex = header.lastIndexOf(':');
    // final hashStart = start + colonIndex;
    // final storedHash = header.substring(colonIndex + 1).trim();
    // final braceStart = text.indexOf("{", lineEnd);
    final braceEnd = findMatchingBrace(text, start, "{", "}")!;
    // final block = text.substring(start, braceEnd + 1);
    final code = text.substring(lineEnd, braceEnd + 1);
    final computedHash = sha256.convert(utf8.encode(code)).toString().substring(0, 7);

    return (
      code: code,
      hashStart: start,
      hashEnd: lineEnd,
      storedHash: storedHash,
      computedHash: computedHash,
    );
  }).where((x) => x.storedHash != x.computedHash)
    .toList();
}

void verifyDirectoryShallow(Directory dir) {
  final entities = dir.listSync(followLinks: false);

  for (final entity in entities) {
    if (entity is! File) continue;

    var text = entity.readAsStringSync();
    bool modified = false;

    while (true) {
      final failures = extractAndVerifyBlocks(text);

      if (failures.isEmpty) {
        break;
      }

      final failure = failures.first;

      print('\n${entity.path}');
      print('--------------------------------------------------');
      print(failure.code);
      print('--------------------------------------------------');
      print('Stored hash : ${failure.storedHash}');
      print('Correct hash: ${failure.computedHash}');
      stdout.write('Update hash? [y=yes, anything else=no] ');

      final response = stdin.readLineSync();

      if (response != 'y') {
        break;
      }

      text =
          text.substring(0, failure.hashStart) +
          failure.computedHash +
          text.substring(failure.hashEnd);

      modified = true;

      print('Hash updated.\n');

      // Loop continues and recomputes all failures from the updated text.
    }

    if (modified) {
      entity.writeAsStringSync(text);
    }
  }
}

void main() {
  verifyDirectoryShallow(Directory("./web/play"));
}