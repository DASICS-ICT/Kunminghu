// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

// Keep the ordinary load fixture unchanged while selecting real MAB services
// and generation-aware architectural retry checks for nonaligned loads.
class FDILoadMisalignBackendTest extends FDILoadPermissionBackendTest(misalignCases = true)
