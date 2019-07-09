#include <iostream>
#include <vector>
#include <stdexcept>
#include <map>
#include <algorithm>

template<typename T>
class Polynomial {
private:
    std::map<size_t, T> data;

    void delete_zeros(std::map<size_t, T> &v) {
        auto it = v.begin();
        while ((it = std::find_if(it, v.end(), [](std::pair<size_t, T> p) {
            return p.second == T(0);
        })) != v.end())
            v.erase(it++);
    }

public:
    explicit Polynomial(const std::vector<T> &input) {
        size_t i = 0;
        for (auto it = input.begin(); it != input.end(); ++it) {
            if (*it != T(0)) {
                data[i] = *it;
                ++i;
            } else {
                ++i;
            }
        }
    }

    explicit Polynomial(const T &scalar = T()) {
        if (scalar == T(0)) {
            data = {};
        } else {
            data[0] = scalar;
        }
    }

    template<typename Iter>
    Polynomial(Iter first, Iter last) {
        size_t i = 0;
        for (auto it = first; it != last; ++it) {
            if (*it != T(0)) {
                data[i] = *it;
                ++i;
            } else {
                ++i;
            }
        }
    }

    bool operator==(const Polynomial &other) const {
        if (data.size() != other.data.size()) {
            return false;
        } else {
            auto jt = other.data.begin();
            for (auto it = data.begin(); it != data.end(); ++it) {
                if (*it != *jt)
                    return false;
                ++jt;
            }
            return true;
        }
    }

    bool operator==(const T &scalar) const {
        return *this == Polynomial(scalar);
    }

    friend bool operator==(const T &scalar, const Polynomial &expr) {
        return expr == Polynomial(scalar);
    }

    bool operator!=(const Polynomial &other) const {
        return !(*this == other);
    }

    bool operator!=(const T &scalar) const {
        return !(*this == Polynomial(scalar));
    }

    friend bool operator!=(const T &scalar, const Polynomial &expr) {
        return !(expr == Polynomial(scalar));
    }

    T operator[](size_t i) const {
        if (data.find(i) == data.end()) {
            return T(0);
        } else {
            return data.at(i);
        }
    }

    int Degree() const {
        if (data.empty()) {
            return -1;
        } else {
            return (--data.end())->first;
        }
    }

    const auto begin() const {
        return data.begin();
    }

    const auto end() const {
        return data.end();
    }

    Polynomial &operator+=(const Polynomial &other) {
        for (auto it = other.data.begin(); it != other.data.end(); ++it) {
            auto[deg, val] = *it;
            data[deg] += val;
        }
        delete_zeros(data);
        return *this;
    }

    Polynomial &operator+=(const T &scalar) {
        return *this += Polynomial(scalar);
    }


    Polynomial &operator-=(const Polynomial &other) {
        for (auto it = other.data.begin(); it != other.data.end(); ++it) {
            auto[deg, val] = *it;
            data[deg] -= val;
        }
        delete_zeros(data);
        return *this;
    }

    Polynomial &operator-=(const T &scalar) {
        return *this -= Polynomial(scalar);
    }

    Polynomial operator+(const Polynomial &other) const {
        Polynomial temp = *this;
        temp += other;
        return temp;
    }

    Polynomial operator+(const T &scalar) const {
        return *this + Polynomial(scalar);
    }

    friend Polynomial operator+(const T &scalar, const Polynomial &expr) {
        return Polynomial(scalar) + expr;
    }

    Polynomial operator-(const Polynomial &other) const {
        Polynomial temp = *this;
        temp -= other;
        return temp;
    }

    Polynomial operator-(const T &scalar) const {
        return *this - Polynomial(scalar);
    }

    friend Polynomial operator-(const T &scalar, const Polynomial &expr) {
        return Polynomial(scalar) - expr;
    }

    Polynomial &operator*=(const Polynomial &other) {
        if (data.empty() || other.data.empty()) {
            data = {};
        } else {
            std::map<size_t, T> temp;
            for (auto it = other.data.begin(); it != other.data.end(); ++it) {
                for (auto jt = data.begin(); jt != data.end(); ++jt) {
                    auto[deg1, val1] = *it;
                    auto[deg2, val2] = *jt;
                    temp[deg1 + deg2] += val1 * val2;
                }
            }
            delete_zeros(temp);
            data = temp;
        }
        return *this;
    }

    Polynomial &operator*=(const T &scalar) {
        if (scalar == T(0)) {
            *this = Polynomial();
        } else {
            (*this) *= Polynomial(scalar);
        }
        return *this;
    }

    Polynomial operator*(const Polynomial &other) const {
        Polynomial<T> temp = *this;
        temp *= other;
        return temp;
    }

    Polynomial operator*(const T &scalar) const {
        Polynomial<T> temp = *this;
        temp *= scalar;
        return temp;
    }

    friend Polynomial operator*(const T &scalar, const Polynomial &other) {
        Polynomial<T> temp = other;
        temp *= scalar;
        return temp;
    }

    T operator ()(const T& scalar) const {
        if (data.empty()) {
            return T(0);
        } else {
            T ans = T(0);
            T temp = T(1);
            size_t ind = 0;
            for (auto it = data.begin(); it != data.end(); ++it) {
                auto[deg, val] = *it;
                while (ind != deg) {
                    temp *= scalar;
                    ++ind;
                }
                ans += val * temp;
            }
            return ans;
        }
    }

    Polynomial operator & (const Polynomial& other) const {
        if (Degree() == -1) {
            return Polynomial();
        } else {
            Polynomial<T> ans;
            Polynomial<T> temp(T(1));
            size_t ind = 0;
            for (auto it = data.begin(); it != data.end(); ++it) {
                auto[deg, val] = *it;
                while (ind != deg) {
                    temp *= other;
                    ++ind;
                }
                ans += val * temp;
            }
            return ans;
        }
    }

    Polynomial operator / (const Polynomial& divider) const {
        if (divider.Degree() == -1) {
            throw std::invalid_argument("Division by zero");
        } else {
            Polynomial<T> ans;
            Polynomial<T> reminder = *this;
            while (reminder.Degree() >= divider.Degree()) {
                Polynomial<T> temp;
                temp.data[reminder.Degree() - divider.Degree()] = reminder[reminder.Degree()]
                        / divider[divider.Degree()];
                ans += Polynomial(temp);
                reminder -= Polynomial(temp) * divider;
            }
            return ans;
        }
    }

    Polynomial operator % (const Polynomial& divider) const {
        if (divider.Degree() == -1) {
            throw std::invalid_argument("Division by zero");
        } else {
            Polynomial<T> ans;
            Polynomial<T> reminder = *this;
            while (reminder.Degree() >= divider.Degree()) {
                Polynomial<T> temp;
                temp.data[reminder.Degree() - divider.Degree()] = reminder[reminder.Degree()]
                                                                  / divider[divider.Degree()];
                ans += Polynomial(temp);
                reminder -= Polynomial(temp) * divider;
            }
            return reminder;
        }
    }

    friend Polynomial operator,(const Polynomial<T>& first, const Polynomial<T>& second) {
        Polynomial<T> ans = first;
        Polynomial<T> copy = second;

        while (!copy.data.empty()) {
            ans = ans % copy;
            std::swap(ans, copy);
        }
        if (!ans.data.empty()) {
            Polynomial<T> temp(ans[ans.Degree()]);
            ans = ans / temp;
        }
        return ans;
    }
};

template<typename T>
std::ostream& operator << (std::ostream& out, const Polynomial<T>& f) {
    if (f.Degree() == -1 || f.Degree() == 0) {
        out << f[0];
    } else if (f.Degree() == 1) {
        if (f[1] == T(1)) {
            out << "x";
        } else if (f[1] == T(-1)) {
            out << "-x";
        } else {
            out << f[1] << "*x";
        }

        if (f[0] != T(0)) {
            if (f[0] > T(0)) {
                out << "+" << f[0];
            } else {
                out << f[0];
            }
        }
    } else {
        for (auto i = f.Degree(); i > -1; --i) {
            if (i == f.Degree()) {
                if (f[i] == T(1)) {
                    out << "x^" << i;
                } else if (f[i] == T(-1)) {
                    out << "-x^" << i;
                } else {
                    out << f[i] << "*x^" << i;
                }
            } else if (f[i] != T(0)) {
                if (i == 1) {
                    if (f[1] == T(1)) {
                        out << "+x";
                    } else if (f[1] == T(-1)) {
                        out << "-x";
                    } else if (f[1] > T(0)) {
                        out << "+" << f[1] << "*x";
                    } else {
                        out << f[1] << "*x";
                    }
                } else if (i == 0) {
                    if (f[0] > T(0)) {
                        out << "+" << f[0];
                    } else {
                        out << f[0];
                    }
                } else {
                    if (f[i] == T(1)) {
                        out << "+x^" << i;
                    } else if (f[i] == T(-1)) {
                        out << "-x^" << i;
                    } else if (f[i] > T(0)) {
                        out << "+" << f[i] << "*x^" << i;
                    } else {
                        out << f[i] << "*x^" << i;
                    }
                }
            }
        }
    }
    return out;
}
