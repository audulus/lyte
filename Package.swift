// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "Lyte",
    platforms: [.macOS(.v13), .iOS(.v16)],
    products: [
        .library(name: "Lyte",
        type: .dynamic, targets: ["Lyte"]),
    ],
    targets: [
        .binaryTarget(
            name: "CLyte",
            url: "https://github.com/audulus/lyte/releases/download/0.8/CLyte.xcframework.zip",
            checksum: "2a16c88e75272797c0b4dd269a3ad12897f54a215f4929ed722b701a5e81ffa6"
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
