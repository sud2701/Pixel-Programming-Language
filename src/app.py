from swiplserver import PrologMQI, PrologThread

with PrologMQI() as mqi:
    with mqi.create_thread() as prolog_thread:
        result = prolog_thread.query("member(X, [color(blue), color(red)])")
        print(result)

[{'X': {'functor': 'color', 'args': ['blue']}},
 {'X': {'functor': 'color', 'args': ['red']}}]