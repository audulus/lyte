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
            url: "https://github.com/audulus/lyte/releases/download/0.5/CLyte.xcframework.zip",
            checksum: "a6510e30495600ab7a87268fe64ae3006ac3b444024fbfabd8df5fbda6747b02"
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
