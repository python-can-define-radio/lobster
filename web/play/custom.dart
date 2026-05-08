/// The main.dart file was getting big, so we moved some arbitrary stuff to this file.
library;


import 'dart:async';
import 'dart:math' show log;
import 'package:meta/meta.dart';


/// Square a number
num sq(num x) => x * x;

double logbase10(double x) => log(x) / log(10);

/// Returns a sub-iterable including all items except the last item.
/// If `orig` is empty, return it.
Iterable<T> withoutLast<T>(Iterable<T> orig) =>
    orig.isEmpty ? orig : orig.take(orig.length - 1);


extension FunctionPipe<T> on T {
    /// Source: https://github.com/dart-lang/language/issues/1246
    R then<R>(R Function (T) f) => f(this);
}


sealed class Result<S, F> {
    /// Apply the function if the result is a success.
    Result<U, F> map<U>(U Function(S) f);
}
class Success<S, F> extends Result<S, F> {
    final S val;
    Success(this.val);
    @override
    Result<U, F> map<U>(U Function(S) f) {
        return Success(f(val));
    }
    
}
class Failure<S, F> extends Result<S, F> {
    final F val;
    Failure(this.val);
    @override
    Result<U, F> map<U>(_) {
        return Failure(val);
    }
}

/// Un-nest Result{Result}. Does only one layer of flattening.
Result<T, U> flatten<T, U>(Result<Result<T, U>, U> r) =>
    switch (r) { 
        Failure(val: final errmsg) => Failure(errmsg),
        Success(val: Failure(val: final errmsg)) => Failure(errmsg),
        Success(val: Success(val: final succval)) => Success(succval),
    };


/// Metadata to mark something as doing some side effect.
/// Unlike Haskell, we're not actively tracking these; it's just
/// a reminder.
/// Use "*" to indicate "all effects allowed". This should probably only be done on main().
class Eff {
    final String desc;
    const Eff(this.desc);
}

/// Metadata to mark something as mutating either its arguments or instance attributes.
/// Unlike Haskell, we're not actively tracking these; it's just
/// a reminder.
class Mut {
    final List<String> mutated;
    const Mut(this.mutated);
}


extension Scanner<T> on Stream<T> {
    Stream<S> scan<S>(S initial, S Function(S, T) combine) async* {
        S prevIteration = initial;
        yield prevIteration;
        await for(final current in this) {
            prevIteration = combine(prevIteration, current);
            yield prevIteration;
        }
    }
}


/// A wrapped stream that keeps a record of the most recent stream value.
class Observable<T> {
    T _latestVal;
    T get latestVal => _latestVal;
    @Mut(["this._latestVal"])
    Observable(this._latestVal, Stream<T> stream) {
        stream.listen((val) => _latestVal = val);
    }
}


Result<T, String> succIf<T>(T val, bool cond, String errmsg) {
    if (cond) {
        return Success(val);
    } else {
        return Failure(errmsg);
    }
}


/// Stream Controller, with access to the Latest Value
class SCoLV<T> {
    final StreamController<T> sc;
    T _latestVal;

    SCoLV(this.sc, this._latestVal);
    
    T get latestVal => _latestVal;

    Stream<T> get stream => sc.stream;

    Observable<T> get observable => Observable(latestVal, stream);

    @factory
    static SCoLV<T> create<T>(T initVal) {
        final sc = StreamController<T>.broadcast();
        return SCoLV(sc, initVal);
    }

    @Mut(["this.sc", "this._latestVal"])
    void set(T val) {
        sc.add(val);
        _latestVal = val;
    }
}

class ImmuSet<T> {
    final Set<T> _wrset;
    ImmuSet(Set<T> elements) : _wrset = Set.unmodifiable(elements);
    bool contains(T value) => _wrset.contains(value);
}

extension type ImmuList<T>._(List<T> values) implements Iterable<T> {
    ImmuList(Iterable<T> vals) : values = List.unmodifiable(vals); 
    /// Same as `append`, but returns a new list.
    ImmuList<T> add(T other) =>
        ImmuList(values + [other]); 
}
