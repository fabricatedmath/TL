#pragma once

class CudaSHA {
public:
    enum Availability {
        Available, NotCompiled, NoNvidiaDriver
    };

    static const char * availabilityString(const Availability availability);
    static Availability check_availablity();
};