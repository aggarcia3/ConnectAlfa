// Código de acción interna para el proyecto ConnectAlfa.mas2j

package esei.si.alejandrogg;

import java.util.Random;
import java.security.SecureRandom;

import jason.*;
import jason.asSemantics.*;
import jason.asSyntax.*;

public final class claveAleatoria extends DefaultInternalAction {
    private static final Random prng = new SecureRandom(); // Números aleatorios de alta entropía
    private static InternalAction instancia = null;

    public static InternalAction create() {
        if (instancia == null) {
            instancia = new claveAleatoria();
        }
        return instancia;
    }

    @Override
    public int getMinArgs() {
        return 1;
    }

    @Override
    public int getMaxArgs() {
        return 1;
    }

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        checkArguments(args);
        // Generar un entero de 64 bits, y reinterpretar sus bits como double
        return un.unifies(args[0], new NumberTermImpl(Double.longBitsToDouble(prng.nextLong())));
    }
}

