// SPDX-License-Identifier: MulanPSL-2.0
#include "VFDIAsyncParentResetHarness.h"
#include "verilated.h"

#include <array>
#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <sstream>
#include <stdexcept>
#include <string>

// The order is an independent literal oracle for the production observation ports.
static constexpr std::array<unsigned, 52> addresses = {
    0xbc4, 0xbc5, 0xbc6, 0x9e1, 0x9e2, 0x9e3, 0x880,
    0x890, 0x891, 0x892, 0x893, 0x894, 0x895, 0x896, 0x897,
    0x898, 0x899, 0x89a, 0x89b, 0x89c, 0x89d, 0x89e, 0x89f,
    0x8a0, 0x8a1, 0x8a2, 0x8a3, 0x8a4, 0x8a5, 0x8a6, 0x8a7,
    0x8a8, 0x8a9, 0x8aa, 0x8ab, 0x8ac, 0x8ad, 0x8ae, 0x8af,
    0x8b0, 0x8b1, 0x8b2, 0x8b3,
    0x8c0, 0x8c1, 0x8c2, 0x8c3, 0x8c4, 0x8c5, 0x8c6, 0x8c7, 0x8c8};

static uint64_t writable_mask(unsigned address) {
    switch (address) {
    case 0xbc4: return 0x7ff;
    case 0x9e1: return 0x7c2;
    case 0x880: return UINT64_C(0xbbbbbbbbbbbbbbbb);
    case 0x8c8: return UINT64_C(0x0001000100010001);
    case 0x8b3: return 7;
    case 0x8b0: case 0x8b1: case 0x8b2: return UINT64_MAX;
    default: return UINT64_MAX ^ UINT64_C(7);
    }
}

class Driver {
    VerilatedContext context;
    VFDIAsyncParentResetHarness dut{&context};
    const bool enabled;
    std::array<uint64_t, 52> expected{};
    uint64_t edges = 0;
    uint64_t effects = 0;

    void require(bool condition, const std::string &message) {
        if (!condition) throw std::runtime_error("edge=" + std::to_string(edges) + " " + message);
    }
    std::array<uint64_t, 52> state() const {
        return {dut.io_state_0, dut.io_state_1, dut.io_state_2, dut.io_state_3,
                dut.io_state_4, dut.io_state_5, dut.io_state_6, dut.io_state_7,
                dut.io_state_8, dut.io_state_9, dut.io_state_10, dut.io_state_11,
                dut.io_state_12, dut.io_state_13, dut.io_state_14, dut.io_state_15,
                dut.io_state_16, dut.io_state_17, dut.io_state_18, dut.io_state_19,
                dut.io_state_20, dut.io_state_21, dut.io_state_22, dut.io_state_23,
                dut.io_state_24, dut.io_state_25, dut.io_state_26, dut.io_state_27,
                dut.io_state_28, dut.io_state_29, dut.io_state_30, dut.io_state_31,
                dut.io_state_32, dut.io_state_33, dut.io_state_34, dut.io_state_35,
                dut.io_state_36, dut.io_state_37, dut.io_state_38, dut.io_state_39,
                dut.io_state_40, dut.io_state_41, dut.io_state_42, dut.io_state_43,
                dut.io_state_44, dut.io_state_45, dut.io_state_46, dut.io_state_47,
                dut.io_state_48, dut.io_state_49, dut.io_state_50, dut.io_state_51};
    }
    void evaluate() {
        dut.eval();
        require(!context.gotFinish(), "unexpected HDL stop or finish");
    }
    void edge() {
        dut.clock = 1;
        context.timeInc(1);
        evaluate();
        ++edges;
        dut.clock = 0;
        context.timeInc(1);
        evaluate();
    }
    void check() {
        evaluate();
        const auto observed = state();
        for (unsigned i = 0; i < addresses.size(); ++i) {
            std::ostringstream message;
            message << "address=0x" << std::hex << addresses[i] << " actual=0x" << observed[i]
                    << " expected=0x" << expected[i];
            require(observed[i] == expected[i], message.str());
        }
    }
    unsigned index(unsigned address) {
        for (unsigned i = 0; i < addresses.size(); ++i) if (addresses[i] == address) return i;
        throw std::runtime_error("address missing from independent oracle");
    }
    void drive(unsigned address, uint64_t value) {
        dut.io_request_bits_instruction = (address << 20) | 0x09173;
        dut.io_request_bits_operation = 9;
        dut.io_request_bits_operand = value;
        dut.io_request_bits_basePc = 0x1000;
        dut.io_request_bits_offset = 0;
        dut.io_request_bits_rob = 4;
        evaluate();
    }
    void idle(unsigned count) {
        for (unsigned n = 0; n < count; ++n) {
            evaluate();
            require(dut.io_writes == 0, "unexpected delayed write");
            require(!dut.io_response_valid, "unexpected delayed response");
            check();
            edge();
        }
    }
    void write(unsigned address, uint64_t value) {
        drive(address, value);
        require(dut.io_request_ready, "CSR input is not ready");
        dut.io_request_valid = 1;
        evaluate();
        require(dut.io_writes == 0, "write happened before acceptance edge");
        edge();
        dut.io_request_valid = 0;
        evaluate();
        require(dut.io_response_valid, "accepted request has no response");
        require(bool(dut.io_response_bits_illegal) == !enabled, "wrong access permission");
        require(!dut.io_response_bits_virtualIllegal, "host access reported virtual illegal");
        const uint64_t pulse = enabled ? UINT64_C(1) << index(address) : 0;
        require(dut.io_writes == pulse, "wrong CSR effect selection");
        edge();
        if (enabled) {
            ++effects;
            expected[index(address)] = value & writable_mask(address);
            if (address == 0xbc4) expected[index(0x9e1)] = expected[index(address)] & 0x7c2;
        }
        check();
        idle(2);
    }
    void assert_reset() {
        // clock is low: this evaluation must not advance any synchronous owner.
        dut.coreReset = 1;
        evaluate();
        check();
        edge();
        expected.fill(0);
        check();
        edge();
        edge();
        dut.coreReset = 0;
        evaluate();
    }

public:
    explicit Driver(bool enabled_value) : enabled(enabled_value) {
        context.assertOn(true);
        dut.clock = 0;
        dut.coreReset = 1;
        dut.io_request_valid = 0;
        dut.io_response_ready = 1;
        dut.io_flush = 0;
        dut.io_flushRob = 4;
        dut.io_trap = 0;
        drive(0x8b0, 0);
        edge();
        edge();
        edge();
        dut.coreReset = 0;
        evaluate();
        idle(3);
    }
    void run() {
        if (enabled) {
            unsigned owners = 0;
            for (const auto address : addresses) {
                if (address == 0x9e1) continue;
                write(address, UINT64_MAX);
                ++owners;
            }
            require(owners == 51, "owner coverage differs from 51");
            for (auto value : expected) require(value != 0, "owner or alias not populated");
            assert_reset();
            idle(4);
            write(0x8b0, UINT64_C(0x1122334455667788));
            drive(0x8b0, UINT64_C(0xaabbccddeeff0011));
            dut.io_request_valid = 1;
            evaluate();
            require(dut.io_request_ready, "pending request not accepted");
            edge();
            dut.io_request_valid = 0;
            evaluate();
            require(dut.io_writes == (UINT64_C(1) << index(0x8b0)), "pending C1 effect absent");
            // Assert asynchronous parent reset before the accepted write's effect edge.
            assert_reset();
            idle(8);
            write(0x8b0, UINT64_C(0x8877665544332211));
            idle(4);
            require(effects == 53, "fresh requests did not each produce exactly one effect");
        } else {
            write(0x8b0, UINT64_MAX);
            assert_reset();
            idle(4);
        }
        dut.final();
        std::cout << "FDI_NATIVE_RESET_RESULT {\"status\":\"PASS\",\"enabled\":"
                  << (enabled ? "true" : "false") << ",\"views\":52,\"owners\":"
                  << (enabled ? 51 : 0) << ",\"edges\":" << edges
                  << ",\"effects\":" << effects << "}" << std::endl;
    }
};

int main(int argc, char **argv) {
    if (argc != 2 || (std::string(argv[1]) != "true" && std::string(argv[1]) != "false")) return 2;
    try {
        Driver driver(std::string(argv[1]) == "true");
        driver.run();
        return 0;
    } catch (const std::exception &error) {
        std::cerr << "FDI native reset failure: " << error.what() << std::endl;
        return 1;
    }
}
