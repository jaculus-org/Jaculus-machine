#include "cfg.h"


namespace jac::cfg::tless {


RegId newTmpId() {
    static RegId id = 1;
    if (id == 0) {
        id = 1;
    }
    return id++;
}


}  // namespace jac::cfg
