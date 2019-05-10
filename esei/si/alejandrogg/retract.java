// Código de acción interna para el proyecto ConnectAlfa.mas2j

package esei.si.alejandrogg;

import java.util.Iterator;
import jason.*;
import jason.asSemantics.*;
import jason.asSyntax.*;
import jason.bb.*;

public final class retract extends DefaultInternalAction {
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
		BeliefBase bb;
		Literal bel;
		Iterator<Literal> it;
		Literal actual;
		Literal aBorrar = null;

		checkArguments(args);

		bel = (Literal) args[0];
		bb = ts.getAg().getBB();

		// Basado en el código del método abolish de Agent.java
		synchronized (bb.getLock()) {
			it = bb.getCandidateBeliefs(bel, un);
			if (it != null) {
				// Encontrar y borrar el primer predicado que no sea una regla y que unifique con el primer argumento
				while (it.hasNext() && aBorrar == null) {
					actual = it.next();
					if (!actual.isRule() && un.clone().unifiesNoUndo(bel, actual)) {
						aBorrar = actual;
					}
				}

				if (aBorrar != null){
					ts.getAg().delBel(aBorrar);
				}
			}
		}

		return true;
    }
}
