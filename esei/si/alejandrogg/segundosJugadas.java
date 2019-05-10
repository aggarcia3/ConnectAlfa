// Código de acción interna para el proyecto ConnectAlfa.mas2j

package esei.si.alejandrogg;

import jason.*;
import jason.asSemantics.*;
import jason.asSyntax.*;

public final class segundosJugadas extends DefaultInternalAction {
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
        // Leer argumento del usuario correspondiente desde el fichero .mas2j
        return un.unifies(args[0], ASSyntax.parseNumber(ts.getSettings().getUserParameter("segundosJugadas")));
    }
}

