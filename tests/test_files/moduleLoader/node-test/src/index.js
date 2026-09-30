import "./relative.js";
import "./implicit.mjs";
import "path";

import "@namespace/package";
import "main-export";
import "exports";
import "exports/aliased";
import "exports/nested/a";
import "exports/extension/x.js";
import "exports/remapped";
import "exports/array";
import "exports/wildcard/features/x";
import "no-package/value.js";

report(import.meta.url);
report(String(import.meta.main));

exit(0);
