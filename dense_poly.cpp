#include <iostream>
#include <vector>
#include <stdexcept>

template<typename T>
class Polynomial {
private:
    std::vector<T> data;

    void cut_vector(std::vector<T> &v) {
        int pos = -1;
        for (size_t i = v.size() - 1; i != size_t(-1); --i) {
            if (v[i] != T(0)) {
                pos = i;
                break;
            }
        }

        if (pos == -1)
            v = {};
        else
            std::vector<T>(v.begin(), v.begin() + pos + 1).swap(v);
    }

public:
    explicit Polynomial(const std::vector<T> &input) {
        data = input;
        cut_vector(data);
    }

    explicit Polynomial(const T &scalar = T()) {
        if (scalar == T(0)) {
            data = {};
        } else {
            data = {scalar};
        }
    }

    template<typename Iter>
    Polynomial(Iter first, Iter last) {
        for (auto it = first; it != last; ++it) {
            data.push_back(*it);
        }
        cut_vector(data);
    }

    bool operator==(const Polynomial &other) const {
        if (data.size() != other.data.size()) {
            return false;
        } else {
            for (size_t i = 0; i < data.size(); ++i) {
                if (data[i] != other.data[i])
                    return false;
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
        if (i > data.size() - 1) {
            return T(0);
        } else {
            return data[i];
        }
    }

    long long int Degree() const {
        if (data.empty()) {
            return -1;
        } else {
            return data.size() - 1;
        }
    }

    const auto begin() const {
        return data.begin();
    }

    const auto end() const {
        return data.end();
    }

    Polynomial &operator+=(const Polynomial &other) {
        data.resize(std::max(data.size(), other.data.size()));
        for (size_t i = 0; i != std::min(data.size(), other.data.size()); ++i) {
            data[i] += other.data[i];
        }
        cut_vector(data);
        return *this;
    }

    Polynomial &operator+=(const T &scalar) {
        return *this += Polynomial(scalar);
    }


    Polynomial &operator-=(const Polynomial &other) {
        data.resize(std::max(data.size(), other.data.size()));
        for (size_t i = 0; i != std::min(data.size(), other.data.size()); ++i) {
            data[i] -= other.data[i];
        }
        cut_vector(data);
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
            *this = Polynomial();
        } else {
            std::vector<T> temp(data.size() + other.data.size() - 1);
            for (size_t i = 0; i != data.size(); ++i) {
                for (size_t j = 0; j != other.data.size(); ++j) {
                    temp[i + j] += (data[i] * other.data[j]);
                }
            }
            *this = Polynomial(temp);
        }
        return *this;
    }

    Polynomial &operator*=(const T &scalar) {
        if (scalar == T(0)) {
            *this = Polynomial();
        } else {
            for (size_t i = 0; i != data.size(); ++i) {
                data[i] *= scalar;
            }
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
            T ans = data[0];
            T temp = scalar;
            for (size_t i = 1; i < data.size(); ++i) {
                ans += temp * data[i];
                temp *= scalar;
            }
            return ans;
        }
    }

    Polynomial power(long long int n) const {
        Polynomial ans = *this;
        for (long long int i = 1; i < n; ++i) {
            ans *= *this;
        }
        return ans;
    }

    Polynomial operator & (const Polynomial& other) const {
        if ((*this).Degree() == -1) {
            return Polynomial();
        } else if (other.Degree() == -1) {
            return Polynomial((*this)[0]);
        } else {
            Polynomial<T> ans((*this)[0]);
            for (long long int i = 1; i <= (*this).Degree(); ++i) {
                ans += (*this)[i] * other.power(i);
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
                std::vector<T> temp(size_t(reminder.Degree() - divider.Degree() + 1));
                temp[temp.size() - 1] = reminder[reminder.Degree()] / divider[divider.Degree()];
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
                std::vector<T> temp(size_t(reminder.Degree() - divider.Degree() + 1));
                temp[temp.size() - 1] = reminder[reminder.Degree()] / divider[divider.Degree()];
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
            Polynomial<T> temp(ans[ans.data.size() - 1]);
            ans = ans /  temp;
        }
        return ans;
    }
};

template<typename T>
std::ostream& operator << (std::ostream& out, const Polynomial<T>& f) {
    if (f.Degree() == -1) {
        out << T(0);
    } else if (f.Degree() == 0) {
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
