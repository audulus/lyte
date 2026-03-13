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
            checksum: "f251c688429a04bf6fde1c82eb9ed5e6dc587cfb00d843f4e5b1f80258146709"
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
