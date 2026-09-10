import "./relative.js";
import "path";

import "@namespace/package";
import "main-export";
import "exports";
import "exports/aliased";
import "exports/nested/a";
import "exports/extension/x.js";
import "remapped";

exit(0);
