#include <algorithm>
#include <cstdint>
#include <iterator>

#include <sort.h>

inline bool operator<(Point x, Point y) {
    return x.id < y.id;
}

namespace std {

namespace sort_helper {

template <typename _Data1, typename _Data2, typename _Order>
struct value_reference_t;

template <typename _Data1, typename _Data2, typename _Order>
struct value_t {
    _Data1 data1;
    _Data2 data2;
    _Order val;
    inline value_t(_Data1 _data1, _Data2 _data2, _Order _val) : data1(_data1), data2(_data2), val(_val) {}
    inline value_t(const value_reference_t<_Data1, _Data2, _Order>& rhs);

    inline bool operator < (const value_t<_Data1, _Data2, _Order>& rhs) const { return val < rhs.val; }
};

template <typename _Data1, typename _Data2, typename _Order>
struct value_reference_t {
    _Data1* pdata1;
    _Data2* pdata2;
    _Order* pval;
    value_reference_t(_Data1* _itData1, _Data2* _itData2, _Order* _itVal)
        : pdata1(_itData1)
        , pdata2(_itData2)
        , pval(_itVal)
        {}
    inline value_reference_t& operator = (const value_reference_t& rhs) {
        *pdata1 = *rhs.pdata1;
        *pdata2 = *rhs.pdata2;
        *pval   = *rhs.pval;
        return *this;
    }
    inline value_reference_t& operator = (const value_t<_Data1, _Data2, _Order>& rhs) {
        *pdata1 = rhs.data1;
        *pdata2 = rhs.data2;
        *pval   = rhs.val;
        return *this;
    }
    inline bool operator < (const value_reference_t& rhs) { return *pval < *rhs.pval; }
};

template <typename _Data1, typename _Data2, typename _Order>
struct value_iterator_t :
    std::iterator<
    random_access_iterator_tag,
    value_t<_Data1, _Data2, _Order>,
    ptrdiff_t,
    value_t<_Data1, _Data2, _Order>*,
    value_reference_t<_Data1, _Data2, _Order>
    >
{
    _Data1* itData1;
    _Data2* itData2;
    _Order* itVal;

    value_iterator_t(_Data1* _itData1, _Data2* _itData2, _Order* _itVal)
        : itData1(_itData1)
        , itData2(_itData2)
        , itVal(_itVal)
        {}

    inline ptrdiff_t operator - (const value_iterator_t& rhs) const { return itVal - rhs.itVal; }
    inline value_iterator_t operator + (ptrdiff_t off) const {
        return value_iterator_t(itData1 + off, itData2 + off, itVal + off);
    }
    inline value_iterator_t operator - (ptrdiff_t off) const {
        return value_iterator_t(itData1 - off, itData2 - off, itVal - off);
    }

    inline value_iterator_t& operator += (ptrdiff_t off) {
        itData1 += off;
        itData2 += off;
        itVal   += off;
        return *this;
    }
    inline value_iterator_t& operator -= (ptrdiff_t off) {
        itData1 -= off;
        itData2 -= off;
        itVal   -= off;
        return *this;
    }

    inline value_iterator_t& operator ++ () { ++itData1; ++itData2; ++itVal; return *this; }
    inline value_iterator_t& operator -- () { --itData1; --itData2; --itVal; return *this; }
    inline value_iterator_t operator ++ (int) { return value_iterator_t(itData1++, itData2++, itVal++); }
    inline value_iterator_t operator -- (int) { return value_iterator_t(itData1--, itData2--, itVal--); }
    inline value_t<_Data1, _Data2, _Order> operator * () const {
        return value_t<_Data1, _Data2, _Order>(*itData1, *itData2, *itVal);
    }
    inline value_reference_t<_Data1, _Data2, _Order> operator * () {
        return value_reference_t<_Data1, _Data2, _Order>(itData1, itData2, itVal);
    }

    inline bool operator <  (const value_iterator_t& rhs) const { return itVal <  rhs.itVal; }
    inline bool operator <= (const value_iterator_t& rhs) const { return itVal <= rhs.itVal; }
    inline bool operator >  (const value_iterator_t& rhs) const { return itVal >  rhs.itVal; }
    inline bool operator >= (const value_iterator_t& rhs) const { return itVal >= rhs.itVal; }
    inline bool operator == (const value_iterator_t& rhs) const { return itVal == rhs.itVal; }
    inline bool operator != (const value_iterator_t& rhs) const { return itVal != rhs.itVal; }
};

template <typename _Data1, typename _Data2, typename _Order>
inline value_t<_Data1, _Data2, _Order>::value_t(const value_reference_t<_Data1, _Data2, _Order>& rhs)
    : data1(*rhs.pdata1), data2(*rhs.pdata2), val(*rhs.pval) {}

template <typename _Data1, typename _Data2, typename _Order>
bool operator < (
    const value_t<_Data1, _Data2, _Order>& lhs,
    const value_reference_t<_Data1, _Data2, _Order>& rhs
    )
{
    return lhs.val < *rhs.pval;
}

template <typename _Data1, typename _Data2, typename _Order>
bool operator < (
    const value_reference_t<_Data1, _Data2, _Order>& lhs,
    const value_t<_Data1, _Data2, _Order>& rhs
    )
{
    return *lhs.pval < rhs.val;
}

template <typename _Data1, typename _Data2, typename _Order>
bool operator < (
    const value_reference_t<_Data1, _Data2, _Order>& lhs,
    const value_reference_t<_Data1, _Data2, _Order>& rhs
    )
{
    return *lhs.pval < *rhs.pval;
}

template <typename _Data1, typename _Data2, typename _Order>
void swap(
    value_reference_t<_Data1, _Data2, _Order> lhs,
    value_reference_t<_Data1, _Data2, _Order> rhs
    )
{
    std::swap(*lhs.pdata1, *rhs.pdata1);
    std::swap(*lhs.pdata2, *rhs.pdata2);
    std::swap(*lhs.pval, *rhs.pval);
}


} // namespace sort_helper

} // namespace std

extern "C" {

    void cplusplus_sort_int64(int64_t* xs, int len) {
        std::sort(xs, xs + len);
    }

    void cplusplus_sort_point(Point* xs, int len) {
        std::sort(xs, xs + len);
    }

    void cplusplus_sort_point_unboxed(double* xs, double* ys, uint64_t* ids, int len) {
        typedef std::sort_helper::value_iterator_t<double, double, uint64_t> PointIterator;

        PointIterator i = PointIterator(xs, ys, ids);
        std::sort(i, i + len);
    }

}
