// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "Lyte",
    platforms: [.macOS(.v13), .iOS(.v16)],
    products: [
        .library(name: "Lyte", targets: ["Lyte"]),
    ],
    targets: [
        .binaryTarget(
            name: "CLyte",
            url: "https://github.com/audulus/lyte/releases/download/0.4/CLyte.xcframework.zip",
            checksum: "bb1ccdfce1417eee779d39715f91ea7b064707e031c03474a9ef679022adab5e"
        ),
        .target(
            name: "Lyte",
            dependencies: ["CLyte"]
        ),
        .testTarget(
            name: "LyteTests",
            dependencies: ["Lyte"]
        ),
    ]
)
