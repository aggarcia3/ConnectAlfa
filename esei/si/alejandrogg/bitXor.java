// Código de acción interna para el proyecto ConnectAlfa.mas2j

package esei.si.alejandrogg;

import jason.*;
import jason.asSemantics.*;
import jason.asSyntax.*;

public final class bitXor extends DefaultInternalAction {
    private static InternalAction instancia = null;

    public static InternalAction create() {
        if (instancia == null) {
            instancia = new bitXor();
		}
        return instancia;
    }

    @Override
	public int getMinArgs() {
        return 3;
    }

    @Override
	public int getMaxArgs() {
        return 3;
    }

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
		boolean toret = true;
		double op1;
		double op2;

		checkArguments(args);

		toret = args[0].isNumeric() && args[1].isNumeric();
		if (toret) {
			op1 = ((NumberTerm) args[0]).solve();
			op2 = ((NumberTerm) args[1]).solve();
			// Hacer operación XOR binaria entre dos double
			toret = un.unifies(args[2], new NumberTermImpl(Double.longBitsToDouble(Double.doubleToRawLongBits(op1) ^ Double.doubleToRawLongBits(op2))));
		}
		
		return toret;
    }
}
