/*
* let's skip NFA and directly build an DFA
* it should be able to recognize such tokens:
*                                            =
*                                            >
*                                            <
*                                            ==
*                                            >=
*                                            <=
*                                            !=
*                                            !==
*                                            ===
* from DFA we can easily construct transition table, which on input 'a' in a state S will go to a new state S'
*                                                                                      ( TT[S, 'a'] = S' )
* */

data class State(val id: Int, val isAccepting: Boolean)

data class Transition(val fromState: State, val toState: State, val onInput: String)

class FiniteAutomata {

    companion object {
        val STARTING_STATE_INDEX = 0
    }

    private val states = listOf(
            State(1, false), State(2, true), State(3, true), State(4, true), State(5, true),
            State(6, true), State(7, true), State(8, true), State(9, false), State(10, false), State(11, true)
    )

    private val transitions = listOf(
            Transition(states[0], states[1], "="),
            Transition(states[0], states[4], ">"),
            Transition(states[0], states[6], "<"),
            Transition(states[0], states[8], "!"),

            Transition(states[1], states[2], "="),

            Transition(states[2], states[3], "="),

            Transition(states[4], states[5], "="),

            Transition(states[6], states[7], "="),

            Transition(states[8], states[9], "="),

            Transition(states[9], states[10], "=")
    )

    fun runFA(input: String) {
        var currentState = states[STARTING_STATE_INDEX]

        input.forEachIndexed { index, inputSymbol ->
            val transition = transitions.firstOrNull {
                it.fromState == currentState && it.onInput == inputSymbol.toString()
            }

            if (transition != null) {
                currentState = transition.toState
            } else {
                if (currentState.isAccepting) {
                    println("Landed in accepting state with id=${currentState.id} pos=$index")
                }
                currentState = states[STARTING_STATE_INDEX]
            }
        }
    }
}