struct {
    bool b;
    char c;
};

std::tuple<bool, char>

struct {
    bool is_char;
    union {
        char c;
        bool b;
    } u;
};

std::variant<bool, char>
std::optional<bool>
