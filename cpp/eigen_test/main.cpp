#include <algorithm>
#include <iostream>
#include <vector>

#include <Eigen/Dense>

#include <opencv2/core.hpp>
//#include <opencv2/highgui.h>
//#include <opencv2/imgproc.hpp>


cv::Mat pad_transform(cv::Mat transform_matrix) {
	// Pad bottom row onto 2x3 transform to make it 3x3

	auto padded = cv::Mat(cv::Mat::eye(3, 3, transform_matrix.type()));
	transform_matrix.copyTo(padded.rowRange(0, 2));

	return padded;
}

cv::Mat unpad_transform(cv::Mat transform_matrix) {
	// Unpad bottom row from 3x3 transform to make it 2x3

	return transform_matrix.rowRange(0, 2);
}

int main()
{
	//std::vector<int> v = {0, 1, 1, 2, 3, 5, 8};
	std::vector<cv::Point2f> points = {cv::Point2f(3, 4), cv::Point2f(5, 8)};

	std::vector<double> v;

	std::transform(points.begin(), points.end(),
			std::back_inserter(v),
			[](auto& p) { return p.ddot(p); });

	//std::cout << cv::norm(points, cv::NORM_L2SQR);

	for (auto& p : points) std::cout << p << " ";
	std::cout << std::endl;

	for (auto& x : v) std::cout << x << " ";
	std::cout << std::endl;

	//Eigen::VectorXf v(2);
	//std::cout << Eigen::squaredNorm(points);
	//std::cout << v.squaredNorm();

	auto mat = cv::Mat::eye(3, 3, CV_32FC1);

	std::cout << mat << std::endl;
	std::cout << unpad_transform(mat) << std::endl;
	std::cout << pad_transform(unpad_transform(mat)) << std::endl;
	std::cout << std::endl;

	auto unpadded = unpad_transform(mat);
	unpadded.at<float>(1, 2) = 3.0;

	std::cout << unpadded << std::endl;
	std::cout << mat << std::endl;
	std::cout << std::endl;

	auto padded = pad_transform(unpadded);
	padded.at<float>(1, 2) = 8.0;

	std::cout << padded << std::endl;
	std::cout << unpadded << std::endl;
	std::cout << std::endl;


	return 0;
}
