// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

// Reuse the real scheduling, CSR recovery and consumer drivers with public
// replay services enabled. The original fixture keeps its default behavior.
class FDILoadReplayBackendTest extends FDILoadPermissionBackendTest(replayCases = true)
