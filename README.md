# Scoped sessions

This snaplet provides a convenient way to handle typed sessions in your snap application.

It uses lenses to separate your session state into smaller parts which can then be individually managed by nested snaplets.
The session state is therefore separated and subsnaplets have no access to the full global session state.
